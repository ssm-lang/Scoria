module Generator where

import Data.List

import Pretty
import Core

import Frontend -- Need to import this because the instance of Res () is in here

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe ()
import Control.Monad.Reader

import System.IO.Unsafe

trace :: [(String, [(String, Type)], [SSMStm])] -> [(String, [(String, Type)], [SSMStm])]
trace x = unsafePerformIO $ putStrLn (printTab x) >> return x

data Program = Program (SSM ())

instance Show Program where
    show (Program p) = showSSM p

type Variables  = [(String, Type)]
type References = [(String, Type)]

instance Arbitrary Type where
    arbitrary = elements [TInt, TBool, Ref TInt, Ref TBool]

printTab :: [(String, [(String, Type)], [SSMStm])] -> String
printTab tab = unlines (map printOne tab)
  where
      printOne :: (String, [(String, Type)], [SSMStm]) -> String
      printOne (n, typs, stmts) = n ++ ": " ++ intercalate " -> " (map show typs) ++ " -> SSM ()"

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
                        body <- fst <$> sized (arbBody
                                                tab
                                                (filter (not . isReference . snd) inp)
                                                (filter (isReference . snd) inp)
                                                0)
                        return (f, inp, body)
                   | (f, as) <- funs
                   ]
        let refs = filter (isReference . snd) $ concat $ map (\(_,x,_) -> x) tab
        main <- createMain refs [] tab 0
        ssm <- forkbdy ("main", [], main) [] []
        return (Program (pureSSM ssm))
     where
       arbBody :: [(String, [(String, Type)], [SSMStm])]  -- ^ (function name, argument types, body)
               -> Variables                               -- ^ Variable names and types
               -> References                              -- ^ Reference names and types
               -> Int                                     -- ^ Fresh name generator
               -> Int                                     -- ^ Size parameter
               -> Gen ([SSMStm], Int)
       arbBody _ _ _ c 0         = return ([], c)
       arbBody tab vars refs c n = frequency $
           -- These are the generators that work regardless of the state of vars & refs
           [ (n, do tp        <- oneof [return TInt, return TBool]
                    e         <- choose (0,3) >>= arbExp tp vars
                    name      <- fresh c
                    (rest,c') <- arbBody tab vars ((name, mkReference tp):refs) (c+1) (n-1)
                    return (NewRef (Fresh name) e : rest, c'))
           ,
             (n, do procs <- procGen tab refs
                    ssms <- sequence $ map (\f -> forkbdy f vars refs) procs
                    (rest,c') <- arbBody tab vars refs c (n-1)
                    return (Fork (map pureSSM ssms):rest, c'))
           , (n, do b   <- arbitrary
                    bs  <- choose (0,3) >>= arbExp TBool vars
                    let size = n `div` 2
                    let size' = if size == 0 then 1 else size -- don't want to generate empty then/else
                    (thn,c') <- arbBody tab vars refs c size'
                    (els,c'') <- if b
                                   then do (els,c'') <- arbBody tab vars refs c' size'
                                           return (Just els, c'')
                                   else return (Nothing, c')
                    (rest,c''') <- arbBody tab vars refs c'' (n `div` 2)
                    return (If bs (pureSSM thn) (fmap pureSSM els):rest, c'''))

           ] ++
           -- These are the generators that only works if refs is not an empty list
           (if null refs then [] else
             [ (n, do r <- elements refs
                      e <- choose (0,3) >>= arbExp (dereference (snd r)) vars
                      (rest,c') <- arbBody tab vars refs c (n-1)
                      return (SetRef r e:rest, c'))
             , (n, do r <- elements refs
                      name <- fresh c
                      (rest,c') <- arbBody tab ((name, dereference (snd r)):vars) refs (c+1) (n-1)
                      return (GetRef r (Fresh name):rest, c'))
             , (n, do delay <- choose (0,3) >>= arbExp TInt vars
                      r <- elements refs
                      e <- choose (0,3) >>= arbExp (dereference (snd r)) vars
                      (rest,c') <- arbBody tab vars refs c (n-1)
                      -- for now, I force the after to be positive and strictly greater than 0
                      -- by multiplying it by itself and adding 1 lol
                      let delay' = BOp TInt (BOp TInt delay delay OTimes) (Lit TInt (LInt 1)) OPlus
                      return (After delay' r e:rest,c'))
             , (n, do r <- elements refs
                      name <- fresh c
                      (rest,c') <- arbBody tab ((name, TBool):vars) refs (c+1) (n-1)
                      return (Changed r (Fresh name):rest,c'))
             , (n, do rs <- sublistOf refs
                      (rest,c') <- arbBody tab vars refs c (n-1)
                      return (Wait rs:rest, c'))
             ]) ++
           -- These are the generators that only works if vars is not an empty list
           (if null vars then [] else
             [ (n, do v <- elements vars
                      e <- choose (0,3) >>= arbExp (snd v) vars
                      (rest,c') <- arbBody tab vars refs c (n-1)
                      let var = Var (snd v) (fst v)
                      return (SetLocal var e:rest,c'))
             ])
        
       fresh :: Int -> Gen String
       fresh n = return $ "v" ++ show n 
       {- Generating random procedures to fork needs to be done carefully. When a fork occurs there
          are a couple of variables (potentially none) that are in scope. The procedures to fork
          all expects some arguments, and we need to make sure that we have enough variables in scope
          with the correct types such that we can apply the procedures when we fork it.
          
          Expressions always work because if we have no variables we can just generate literals, but
          we need to doublecheck the references. This is what this function does. It also guarantees
          that the resulting list is not empty. -}
       procGen :: [(String, [(String, Type)], [SSMStm])]
               -> References 
               -> Gen [(String, [(String, Type)], [SSMStm])]
       procGen tab refs = listOf (elements tab `suchThat` canBeCalled) `suchThat` (not . null)
         where
             knownRefTyps :: [Type]
             knownRefTyps = map snd refs

             refTypes :: [(String, Type)] -> [(String, Type)]
             refTypes typs = filter (isReference . snd) typs

             canBeCalled :: (String, [(String, Type)], [SSMStm]) -> Bool
             canBeCalled (_,typs,_) = all (\t -> snd t `elem` knownRefTyps) $ refTypes typs

       createMain :: [(String, Type)]
                  -> References
                  -> [(String, [(String, Type)], [SSMStm])]
                  -> Int
                  -> Gen [SSMStm]
       createMain [] refs tab c         = fst <$> sized (arbBody tab [] refs c)
       createMain ((x,t):xs) refs tab c = do
           e    <- choose (0,3) >>= arbExp (dereference t) []
           name <- fresh c
           (:) (NewRef (Fresh name) e) <$> createMain xs ((name,t):refs) tab (c+1)

       forkbdy :: (String, [(String, Type)], [SSMStm])
               -> Variables
               -> References
               -> Gen [SSMStm]
       forkbdy (n, typs, bdy) vars refs = do
           (:) (Procedure n) <$> argsAndBody typs
         where
             argsAndBody :: [(String, Type)]  -- ^ Procedure arguments (names and types)
                         -> Gen [SSMStm]
             argsAndBody []         = return $ bdy ++ [Result n]
             argsAndBody ((x,t):ts) = case t of
                     Ref _ -> do r <- elements (filter (\(_,t') -> t == t') refs)
                                 (:) (Argument n x (Right r)) <$> argsAndBody ts
                     _     -> do e <- choose (0,3) >>= arbExp t vars
                                 (:) (Argument n x (Left e)) <$> argsAndBody ts

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
                               return $ BOp TBool e1 e2 OLT
                          , do typ <- elements [TInt, TBool]
                               e1 <- arbExp typ vars (n `div` 2)
                               e2 <- arbExp typ vars (n `div` 2)
                               return $ BOp TBool e1 e2 OEQ]