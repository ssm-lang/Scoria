module LowGenerator where
{-
import Data.List
import Data.Maybe

import Pretty
import Core

import Frontend -- Need to import this because the instance of Res () is in here

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe ()
import Control.Monad.Reader

import System.IO.Unsafe

import CodeGen

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

render :: Program -> Program
render (Program ssm) = unsafePerformIO $ putStrLn (showSSM ssm) >> return (Program ssm)

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
    shrink    = shrinkSSM -- a -> [a]
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
                      return (GetRef (Fresh name) r:rest, c'))
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
                      return (Changed (Fresh name) r:rest,c'))
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

{-********** Shrinking **********-}

testprogram :: Program
testprogram = Program $ pureSSM stmts
  where
    stmts = [ Procedure "test"
            --, Argument "test" "r" (Right ("r",intref))
            , NewRef (Fresh "v0") 3
            , GetRef (Fresh "v1") ("v0", intref)
            , NewRef (Fresh "v2") 5
            , Wait [("v0",intref), ("v2",intref), ("r", intref)]
            , Changed (Fresh "v3") ("v0", intref)
            , If (BOp bool 3 5 OLT)
                (pureSSM [Wait [("v2", intref)]])
                (Just $ pureSSM [Wait [("v0", intref)]])
            --, Fork [pureSSM ([ Procedure "fun1"
            --                 , Argument "fun1" "x" (Left (Var int "v1"))
            --                 , Argument "fun1" "y" (Right ("v0", intref))
            --                 , Wait [("v0", intref)]
            --                 , After 5 ("v0", intref) (Var int "x")
            --                 , GetRef (Fresh "v3") ("v0", intref)
            --                 , Result "fun1"
            --                 ])]
            , Result "test"
            ]
    
    int     = TInt
    bool    = TBool
    intref  = Ref int
    boolref = Ref bool

testprogram2 :: Program
testprogram2 = Program $ pureSSM stmts
  where
    stmts = [ Procedure "test"
            , NewRef (Fresh "v0") 3
            , NewRef (Fresh "v2") 5
            , If (Lit bool $ LBool True)
                (pureSSM [Wait [("v2", intref), ("v0", intref)]])
                (Just $ pureSSM [Wait [("v2", intref),("v0", intref)]])
            , Result "test"
            ]
    
    int     = TInt
    bool    = TBool
    intref  = Ref int
    boolref = Ref bool

testprogram3 :: Program
testprogram3 = Program $ pureSSM stmts
  where
    stmts = [ Procedure "test"
            , NewRef (Fresh "v0") 5
            , NewRef (Fresh "v2") (2 - 0)
            , GetRef (Fresh "v3") ("v2", intref)
            , Fork [ pureSSM $ [ Procedure "fun3"
                               , Argument "fun3" "var1" (Left (0 - 0))
                               , Argument "fun3" "var2" (Left (-6))
                               , Argument "fun3" "ref3" (Right ("v2", intref))
                               , Argument "fun3" "ref4" (Right ("v0", intref))
                               , Argument "fun3" "var4" (Left ((Var int "v3") - (Var int "v3")))
                               , Result "fun3"] ]
            , Result "test"
            ]
    
    int     = TInt
    bool    = TBool
    intref  = Ref int
    boolref = Ref bool

{- | Takes a program and returns all subprograms where each subprogram contains only
a permutation of the reference declarations. -}
shrinkSSM :: Program -> [Program]
shrinkSSM (Program main) = map (Program . pureSSM) $ removeReferences $ runSSM main

-- | Return all subprograms where one or more of the declared references are removed.
removeReferences :: [SSMStm] -> [[SSMStm]]
removeReferences stmts = let maybs = map (\p -> removeRef p [] [] stmts) (refs stmts)
                         in map fromJust $ filter isJust maybs

refs :: [SSMStm] -> [String]
refs stmts = declaredRefs stmts

-- | Fetch all declared references in a program
declaredRefs :: [SSMStm] -> [String]
declaredRefs []                      = []
declaredRefs (NewRef (Fresh n) e:xs) = n : declaredRefs xs
declaredRefs (_:xs)                  =     declaredRefs xs

removeRef :: String       -- ^ Name of the references we wish to remove from the program
          -> [String]     -- ^ Variables that are no longer valid (since e.g we did GetRef on a ref)
          -> [Reference]  -- ^ References that are valid
          -> [SSMStm]     -- ^ The program to transform
          -> Maybe [SSMStm]
removeRef bad vars refs []     = Just []
removeRef bad vars refs (x:xs) = case x of
  NewRef (Fresh n) e      | n == bad ->     removeRef bad vars refs xs
                          | otherwise    -> let x' = NewRef (Fresh n) $ deleteVars vars e
                                                r  = (n, mkReference (expType e))
                                            in (:) x' <$> removeRef bad vars (r:refs) xs
  GetRef (Fresh n) (r,t)  | r == bad ->     removeRef bad (n:vars) refs xs
                          | otherwise    -> (:) x <$> removeRef bad vars refs xs
  SetRef (r,t) e          | r == bad ->     removeRef bad vars refs xs
                          | otherwise    -> let x' = SetRef (r,t) $ deleteVars vars e
                                            in (:) x' <$> removeRef bad vars refs xs 
  After d (r,t) e         | r == bad ->     removeRef bad vars refs xs
                          | otherwise    -> let d' = deleteVars vars d'
                                                e' = deleteVars vars e
                                                x' = After d' (r,t) e'
                                            in (:) x' <$> removeRef bad vars refs xs
  Changed (Fresh n) (r,_) | r == bad ->     removeRef bad (n:vars) refs xs
                          | otherwise    -> (:) x <$> removeRef bad vars refs xs
  Wait waits -> let res = filter ((/=) bad . fst) waits
                in if null res
                   then Nothing --removeRef bad vars refs xs
                   else (:) (Wait res) <$> removeRef bad vars refs xs
  
  SetLocal (Var t n) e2 | n `elem` vars -> removeRef bad vars refs xs
                        | otherwise     -> let x' = SetLocal (Var t n) $ deleteVars vars e2
                                           in (:) x' <$> removeRef bad vars refs xs
  If c thn els   -> let c'   = deleteVars vars c
                        thn' = pureSSM <$> removeRef bad vars refs (runSSM thn)
                        els' = case els of
                          Just b -> Just $ pureSSM <$> removeRef bad vars refs (runSSM b)
                          Nothing -> Nothing 
                        x' = If c' <$> thn' <*> els'
                    in  case els' of
                      Just Nothing -> Nothing
                      Just _       -> (:) <$> x' <*> removeRef bad vars refs xs
  While c bdy    -> let c'   = deleteVars vars c
                        bdy' = fmap pureSSM $ removeRef bad vars refs $ runSSM bdy
                        x'   = While c' <$> bdy'
                    in  (:) <$> x' <*> removeRef bad vars refs xs
  Fork procs     -> let mayb   = map (deleteRefsFromFork vars bad refs) procs
                        procs' = map fromJust $ filter isJust mayb
                    in if null procs'
                       then removeRef bad vars refs xs
                       else (:) (Fork procs') <$> removeRef bad vars refs xs
  Argument _ _ (Right r) -> (:) x <$> removeRef bad vars (r:refs) xs

  _ -> (:) x <$> removeRef bad vars refs xs
  where
    {- | Given a list of variable names and an expression, returns the same expression but where
    every occurence of one of the variables in the list is replaced with a default value. -}
    deleteVars :: [String]  -- ^ Names of variables that were invalidated when removing references
               -> SSMExp    -- ^ An expression from which to delete all occurences of the variables
               -> SSMExp
    deleteVars vars e = case e of
      Var t n | n `elem` vars -> defaultValue t
      UOp t e op              -> UOp t (deleteVars vars e) op
      BOp t e1 e2 op          -> BOp t (deleteVars vars e1) (deleteVars vars e2) op
      _                       -> e
      where
        defaultValue :: Type -> SSMExp
        defaultValue TInt  = Lit TInt  $ LInt 0
        defaultValue TBool = Lit TBool $ LBool False
    
    {- | Tries to rewrites a forked procedure. It might have been applied to a reference that
    is no longer valid or expressions that contains variables that are no longer valid.
    These arguments must be rewritten. In the case of a reference being used that no longer
    exists it will try to pick another one of the same type. If none exists, the fork is
    no longer possible. -}
    deleteRefsFromFork :: [String]     -- ^ Names of variables that are invalid and should be replaced
                       -> String       -- ^ Name of references to remove
                       -> [Reference]  -- ^ References that are valid
                       -> SSM ()       -- ^ The forked procedure to possibly give new arguments
                       -> Maybe (SSM ())
    deleteRefsFromFork vars bad refs = fmap pureSSM . rewriteHeader . runSSM
      where
        rewriteHeader :: [SSMStm] -> Maybe [SSMStm]
        rewriteHeader []                  = Just []
        rewriteHeader (Procedure n:xs)    = (:) (Procedure n) <$> rewriteHeader xs
        rewriteHeader (Argument n x (Left e):xs) =
          (:) (Argument n x (Left (deleteVars vars e))) <$> rewriteHeader xs
        rewriteHeader (Argument n x (Right (r,t)):xs)
          | r == bad = case findNewRef t refs of
            Just r' -> (:) (Argument n x (Right r'))    <$> rewriteHeader xs
            Nothing -> Nothing
          | otherwise = (:) (Argument n x (Right (r,t))) <$> rewriteHeader xs
        rewriteHeader inp@(_:xs)              = Just inp

        findNewRef :: Type -> [Reference] -> Maybe Reference
        findNewRef _ []            = Nothing
        findNewRef t (r@(_,t'):xs) = if t == t' then Just r else findNewRef t xs -}