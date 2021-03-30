module Generator where

import Data.List

import Pretty
import Core

import Frontend () -- Need to import this because the instance of Res () is in here

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe (promote)
import Control.Monad.Reader

import System.IO.Unsafe

trace :: SSM () -> SSM ()
trace x = unsafePerformIO $ putStrLn (showSSM x) >> return x

data Program = Program (SSM ())

instance Show Program where
    show (Program p) = showSSM p

type Variables  = [(String, Type)]
type References = [(String, Type)]

instance Arbitrary Type where
    arbitrary = elements [TInt, TBool, Ref TInt, Ref TBool]

instance Arbitrary Program where
    arbitrary = do
        types <- arbitrary `suchThat` (not . null)
        let funs = [ ("fun" ++ show i, as) | (as,i) <- types `zip` [1..] ]
        
        tab <- mfix $ \tab -> sequence [ do
                        {- I changed the second component of an entry in tab to be of type
                           (String, Type) instead of just Type, as when I create the Argument
                           constructors I want to supply the name of the argument as it is
                           inserted by the box typeclass. -}
                        let inp = [ ((if isReference a then "ref" else "var") ++ show i, a)
                                  | (a,i) <- as `zip` [1..]
                                  ]
                        body <- sized $ arbBody
                                            tab
                                            (filter (not . isReference . snd) inp)
                                            (filter (isReference . snd) inp)
                        return (f, inp, body)
                   | (f, as) <- funs
                   ]
        let refs = filter (isReference . snd) $ concat $ map (\(_,x,_) -> x) tab
        main <- createMain refs [] tab
        ssm <- forkbdy ("main", [], main) [] []
        return (Program ssm)
     where
       arbBody :: [(String, [(String, Type)], SSM ())]  -- ^ (function name, argument types, body)
               -> Variables                             -- ^ Variable names and types
               -> References                            -- ^ Reference names and types
               -> Int                                   -- ^ Size parameter
               -> Gen (SSM ())
       arbBody tab vars refs n = frequency $
           -- These are the generators that work regardless of the state of vars & refs
           [ (1, return (Return ()))

           , (n, do tp <- oneof [return TInt, return TBool]
                    e  <- choose (0,3) >>= arbExp tp vars
                    k  <- promote $ \r -> arbBody tab vars (r:refs) (n-1)
                    return $ NewRef Nothing e k)

           , (n, do procs <- procGen tab refs
                    ssms <- sequence $ map (\f -> forkbdy f vars refs) procs
                    k <- promote $ \() -> arbBody tab vars refs (n-1)
                    return $ Fork ssms k)

           , (n, do b   <- arbitrary
                    bs  <- choose (0,3) >>= arbExp TBool vars
                    thn <- arbBody tab vars refs (n `div` 2)
                    els <- if b
                             then Just <$> arbBody tab vars refs (n `div` 2)
                             else return Nothing
                    k   <- promote $ \() -> arbBody tab vars refs ((n `div` 2))
                    return $ If bs thn els k)
           ] ++
           -- These are the generators that only works if refs is not an empty list
           (if null refs then [] else
             [ (n, do r <- elements refs
                      e <- choose (0,3) >>= arbExp (dereference (snd r)) vars
                      k <- arbBody tab vars refs (n-1)
                      return $ SetRef r e (const k))

             , (n, do r <- elements refs
                      k <- promote $ \v -> do
                          k <- promote $ \ref -> arbBody tab vars (ref:refs) (n-1)
                          return $ NewRef Nothing v k
                      return $ GetRef r Nothing k)
             , (n, do delay <- choose (0,3) >>= arbExp TInt vars
                      r <- elements refs
                      e <- choose (0,3) >>= arbExp (dereference (snd r)) vars
                      k <- promote $ \() -> arbBody tab vars refs (n-1)
                      -- for now, I force the after to be positive by multiplying it by itself
                      return $ After delay r (BOp TInt e e OTimes) k)

             , (n, do r <- elements refs
                      k <- promote $ \v -> do
                          let t = expType v
                          k <- promote $ \ref -> arbBody tab vars (ref:refs) (n-1)
                          return $ NewRef Nothing v k
                      return $ Changed r Nothing k)
             , (n, do rs <- sublistOf refs
                      k <- promote $ \() -> arbBody tab vars refs (n-1)
                      return $ Wait rs k)
             ]) ++
           -- These are the generators that only works if vars is not an empty list
           (if null vars then [] else
             [ (n, do v <- elements vars
                      e <- choose (0,3) >>= arbExp (snd v) vars
                      k <- promote $ \r -> arbBody tab vars refs (n-1)
                      let var = Var (snd v) (fst v)
                      return $ SetLocal var e k)
             ])
        
       {- Generating random procedures to fork needs to be done carefully. When a fork occurs there
          are a couple of variables (potentially none) that are in scope. The procedures to fork
          all expects some arguments, and we need to make sure that we have enough variables in scope
          with the correct types such that we can apply the procedures when we fork it.
          
          Expressions always work because if we have no variables we can just generate literals, but
          we need to doublecheck the references. This is what this function does. It also guarantees
          that the resulting list is not empty. -}
       procGen :: [(String, [(String, Type)], SSM ())]
               -> References 
               -> Gen [(String, [(String, Type)], SSM ())]
       procGen tab refs = listOf (elements tab `suchThat` canBeCalled) `suchThat` (not . null)
         where
             knownRefTyps :: [Type]
             knownRefTyps = map snd refs

             refTypes :: [(String, Type)] -> [(String, Type)]
             refTypes typs = filter (isReference . snd) typs

             canBeCalled :: (String, [(String, Type)], SSM ()) -> Bool
             canBeCalled (_,typs,_) = all (\t -> snd t `elem` knownRefTyps) $ refTypes typs

       createMain :: [(String, Type)]
                  -> References
                  -> [(String, [(String, Type)], SSM ())]
                  -> Gen (SSM ())
       createMain [] refs tab         = sized $ arbBody tab [] refs
       createMain ((x,t):xs) refs tab = do
           e <- choose (0,3) >>= arbExp (dereference t) []
           k <- promote $ \r -> createMain xs (r:refs) tab
           return $ NewRef Nothing e k

       forkbdy :: (String, [(String, Type)], SSM ())
               -> Variables
               -> References
               -> Gen (SSM ())
       forkbdy (n, typs, bdy) vars refs = do
           k <- promote $ \() -> argsAndBody typs
           return $ Procedure n k
         where
             argsAndBody :: [(String, Type)]  -- ^ Procedure arguments (names and types)
                         -> Gen (SSM ())
             argsAndBody []         = return $ bdy >>= \() -> Result n () return
             argsAndBody ((x,t):ts) = case t of
                     Ref _ -> do r <- elements (filter (\(_,t') -> t == t') refs)
                                 k <- promote $ \() -> argsAndBody ts
                                 return $ Argument n x (Right r) k
                     _     -> do e <- choose (0,3) >>= arbExp t vars
                                 k <- promote $ \() -> argsAndBody ts
                                 return $ Argument n x (Left e) k

       arbExp :: Type              -- ^ Type of expression to generate
              -> [(String, Type)]  -- ^ Variables the expression is allowed to use
              -> Int               -- ^ Size parameter
              -> Gen SSMExp
       arbExp t vars 0 = case t of
           -- In the base case we want either a literal or a variable
           TInt  -> oneof $ (Lit TInt . LInt <$> arbitrary)   : [ return (Var t n) | (n,t) <- vars
                                                                                   , t == TInt]
           TBool -> oneof $ (Lit TBool . LBool <$> arbitrary) : [ return (Var t n) | (n,t) <- vars
                                                                                   , t == TBool]
       arbExp t vars n = case t of
           -- In the inductive case we want recursive expressions
           TInt  -> frequency [ (1, do e <- arbExp t vars (n-1)
                                       return $ UOp t e Neg)
                              , (7, do e1 <- arbExp t vars (n `div` 2) -- 7 arbitrarily chosen
                                       e2 <- arbExp t vars (n `div` 2)
                                       elements [ BOp t e1 e2 OPlus
                                               , BOp t e1 e2 OMinus
                                               , BOp t e1 e2 OTimes
                                               ])
                               ]
           TBool -> oneof [ do e1 <- arbExp TInt vars (n `div` 2)
                               e2 <- arbExp TInt vars (n `div` 2)
                               return $ BOp t e1 e2 OLT
                          , do typ <- elements [TInt, TBool]
                               e1 <- arbExp typ vars (n `div` 2)
                               e2 <- arbExp typ vars (n `div` 2)
                               return $ BOp typ e1 e2 OEQ]