module Generator where

import Data.List

import Pretty
import Core

import Frontend () -- Need to import this because the instance of Res () is in here

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe (promote)
import Control.Monad.Reader

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
        -- I suppose what we want to do here is decide which function is going to be the
        -- entrypoint of the entire program, and return that. There's a helper function
        -- forkbdy that takes one of the triples above and inserts the box constructors.
        bdy <- sized $ arbBody tab [] []
        ssm <- forkbdy ("main", [], bdy) [] []
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
                    e  <- sized $ arbExp tp vars
                    k  <- promote $ \r -> arbBody tab vars (r:refs) (n-1)
                    return $ NewRef Nothing e k)

             {- What I did here for fork is that I just assumed that I could select an arbitrary
                number of procedures from tab to fork. These just have their bodies right now, and
                I need to wrap them in Procedure -> Argument -> Argument -> ... -> Result (),
                so I wrote a helper method that does that. That method also generates the random
                arguments we will apply the procedure to. -}
           , (n, do --procs <- sublistOf tab -- want to allow duplicates
                    procs <- listOf (elements tab) `suchThat` (not . null)
                    ssms <- sequence $ map (\f@(n,inp,bdy) -> forkbdy f vars refs) procs
                    k <- promote $ \() -> arbBody tab vars refs (n-1)
                    return $ Fork ssms k)

           , (n, do b   <- arbitrary
                    bs  <- sized $ arbExp TBool vars
                    thn <- arbBody tab vars refs (n `div` 2)
                    els <- if b
                             {- Here I arbitrarily chose that the else and then branches should
                                be allowed to consume one fifth of the 'size' each, but I did
                                not think that much about it. It was just something I chose so
                                that I could continue. -}
                             then Just <$> arbBody tab vars refs (n `div` 2)
                             else return Nothing
                    k   <- promote $ \() -> arbBody tab vars refs ((n `div` 2))
                    return $ If bs thn els k)
           ] ++
           -- These are the generators that only works if refs is not an empty list
           (if null refs then [] else
             [ (n, do r <- elements refs
                      e <- sized $ arbExp (dereference (snd r)) vars
                      k <- arbBody tab vars refs (n-1)
                      return $ SetRef r e (const k))

             , (n, do r <- elements refs
                      k <- promote $ \(Var t v) -> arbBody tab ((v,t):vars) refs (n-1)
                      return $ GetRef r Nothing k)

             , (n, do delay <- sized $ arbExp TInt vars -- semantics should be able to handle negative delays
                      r <- elements refs
                      e <- sized $ arbExp (dereference (snd r)) vars
                      k <- promote $ \() -> arbBody tab vars refs (n-1)
                      return $ After delay r e k)

             , (n, do r <- elements refs
                      k <- promote $ \(Var t v) -> arbBody tab ((v,t):vars) refs (n-1)
                      return $ Changed r Nothing k)

             , (n, do rs <- sublistOf refs
                      k <- promote $ \() -> arbBody tab vars refs (n-1)
                      return $ Wait rs k)
             ]) ++
           -- These are the generators that only works if vars is not an empty list
           (if null vars then [] else
             [ (n, do v <- elements vars
                      e <- sized $ arbExp (snd v) vars
                      k <- promote $ \r -> arbBody tab vars refs (n-1)
                      let var = Var (snd v) (fst v)
                      return $ SetLocal var e k)
             ])

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
                     Ref _ -> do r <- elements refs
                                 k <- promote $ \() -> argsAndBody ts
                                 return $ Argument n x (Right (x,t)) k
                     _     -> do e <- sized $ arbExp t vars
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
    TBool -> do e1 <- arbExp t vars (n `div` 2)
                e2 <- arbExp t vars (n `div` 2)
                elements [ BOp t e1 e2 OLT
                        , BOp t e1 e2 OEQ]