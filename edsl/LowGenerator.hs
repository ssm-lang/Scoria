{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module LowGenerator where

import Data.List
import Data.Word
import Data.Int
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import LowCore
import HughesList

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe ()
import Control.Monad.Reader
--import Control.Monad.Writer
import Control.Monad.State

import System.IO.Unsafe

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

for :: [a] -> (a -> b) -> [b]
for = flip map

instance Arbitrary Type where
  arbitrary = elements [TInt32, TInt64, TBool, TUInt64, Ref TInt32, Ref TInt64, Ref TUInt64, Ref TBool]

type Procedures = [(String, [(String, Type)], [Stm])]
type Variable = (Name, Type)
type Ref     = (String, Type)

genListOfLength :: Gen a -> Int -> Gen [a]
genListOfLength ga 0 = return []
genListOfLength ga n = (:) <$> ga <*> genListOfLength ga (n-1)

instance Arbitrary Program where
  shrink p = let p' = removeUnusedProcedures p
             in concat [ shrinkHalfProcedures p'
                       , shrinkSingleProcedures p'  -- Shrink number of functions
                       , shrinkArity p'             -- Shrink procedure arity (writing this right now)
                       , shrinkAllStmts p'          -- Shrink by removing statements that effectively
                                                -- have type () (fork, wait etc).
                       , shrinkForks p'             -- Shrink fork statements (fork less things)
                       , shrinkIf p'                -- Flatten if's (every if becomes two new programs)
                       , shrinkRefs p'              -- Shrink number of declared refs
                       , shrinkWait p'              -- Shrink wait statements
                       ]
  arbitrary = do
    let typesiggen = genListOfLength arbitrary =<< choose (0,10)
    types <- genListOfLength typesiggen =<< choose (1,10)
--    types <- take 15 <$> arbitrary `suchThat` (not . null)
    let funs = [ ("fun" ++ show i, as) | (as,i) <- zip types [1..]]
    tab <- mfix $ \tab -> sequence
        [ do let (refs, vars) = partition (isReference . fst) $ zip as [1..]
             let inprefs      = [ ("ref" ++ show i        , t) | (t,i) <- refs]
             let inpvars      = [ (Fresh $ "var" ++ show i, t) | (t,i) <- vars]

             (body,_)        <- arbProc tab inpvars inprefs 0 =<< choose (0, 25)

             let params = [ (if isReference a
                             then "ref"  ++ show i
                             else "var"  ++ show i
                            , a) 
                          | (a,i) <- zip as [1..]]
             return (f, params, body)
        | (f,as) <- funs]
    
    (entrypoint, argtypes) <- elements $ map (\(name,t,_) -> (name, t)) tab
    args <- flip mapM argtypes $ \(n,t) -> if isReference t
      then return $ Right (n,t)
      else do e <- arbExp t [] 1
              return $ Left e
    
    let funs = Map.fromList $ [ (fun, Procedure fun params bdy)
                              | (fun, params, bdy) <- tab]
    
    return $ Program entrypoint args funs

-- | Generate a procedure body.
arbProc :: Procedures     -- ^ All procedures in the program
        -> [Variable]     -- ^ Variables in scope
        -> [Ref]          -- ^ References in scope
        -> Int            -- ^ Fresh name generator
        -> Int            -- ^ Size parameter
        -> Gen ([Stm], Int)
arbProc _ _ _ c 0          = return ([], c)
arbProc funs vars refs c n = frequency $
      [ (1, do t         <- elements [TInt32, TBool]
               e         <- choose (0,3) >>= arbExp t vars
               (name,c1) <- fresh c
               let rt     = mkReference t
               let stm    = NewRef name rt e
               let ref    = (getVarName name, rt)
               (rest, c2) <- arbProc funs vars (ref:refs) c1 (n-1)
               return (stm:rest, c2)
        )
      
      , (1, do cond      <- choose (0,3) >>= arbExp TBool vars
               (thn,c1)  <- arbProc funs vars refs c (n `div` 2)
               (els,c2)  <- arbProc funs vars refs c1 (n `div` 2)
               (rest,c3) <- arbProc funs vars refs c2 (n-1)
               let stm    = If cond thn els
               return (stm:rest, c3)
        )
      
      --, (1, undefined {-while-}
      --  )

      , (1, do let forkable = elements funs `suchThat` canBeCalled refs
               tofork     <- genListOfLength forkable 10 `suchThat` (not . null)
--               tofork      <- listOf forkable `suchThat` (not . null)
               forks       <- mapM (applyFork vars refs) tofork
               let stm      = Fork forks
               (rest,c') <- arbProc funs vars refs c (n-1)
               return (stm:rest, c')
        )
      ] ++

      (if null vars then [] else
      [ (1, do (name,t) <- elements vars
               e        <- choose (0,3) >>= arbExp t vars
               (rest,c') <- arbProc funs vars refs c (n-1)
               let stm   = SetLocal name t e
               return (stm:rest, c')
        )]) ++

      (if null refs then [] else
      [ (1, do r@(_,t)   <- elements refs
               (name,c1) <- fresh c
               let t'    = dereference t
               (rest,c2) <- arbProc funs ((name, t'):vars) refs c1 (n-1)
               let stm   = GetRef name t' r
               return (stm:rest, c2)
        )
      
      , (1, do r@(_,t) <- elements refs
               e       <- choose (0,3) >>= arbExp (dereference t) vars
               (rest,c') <- arbProc funs vars refs c (n-1)
               let stm = SetRef r e
               return (stm:rest, c')
        )
      
      , (1, do r@(_,t)  <- elements refs
               v        <- choose (0,3) >>= arbExp (dereference t) vars
               delay    <- Lit TUInt64 . LUInt64 <$> choose (0, 5000)
               (rest,c') <- arbProc funs vars refs c (n-1)
               let stm   = After delay r v
               return (stm:rest, c')
        )
      
      , (1, do r         <- elements refs
               (name,c1) <- fresh c
               (rest,c2) <- arbProc funs ((name, TBool):vars) refs c1 (n-1)
               let stm    = Changed name TBool r
               return (stm:rest, c2)
        )
      
      , (1, do refs'  <- sublistOf refs `suchThat` (not . null)
               (rest,c') <- arbProc funs vars refs c (n-1)
               let stm = Wait refs'
               return (stm:rest, c')
        )
      ])
  where
      -- | Generate a fresh name.
      fresh :: Monad m => Int -> m (Name, Int)
      fresh c = return $ (Fresh ("v" ++ show c), c+1)

      -- | Take a procedure that should be forked and return the application of
      -- that procedure to randomly generated arguments.
      applyFork :: [Variable]
                -> [Ref]
                -> (String, [(String, Type)], [Stm])
                -> Gen (String, [Either SSMExp Reference])
      applyFork vars refs (n, types, _) = do
          args <- forM types $ \(_,t) ->
            if isReference t
              then do let okrefs = filter ((==) t . snd) refs
                      Right <$> elements okrefs
              else Left <$> (choose (0,3) >>= arbExp t vars)
          return (n, args)

      -- | Predicate that returns True if the given procedure can be forked.
      -- What determines this is what references we have in scope. If the procedure
      -- requires a reference parameter that we do not have, we can not fork it.
      canBeCalled :: [Ref] -> (String, [(String, Type)], [Stm]) -> Bool
      canBeCalled inscope (_, types, _) =
          let distinct = nub $ filter isReference $ map snd inscope
          in all (`elem` distinct) $ filter isReference $ map snd types

-- | Generate a SSMExp.
arbExp :: Type        -- ^ Type of expression to generate (oneof TInt32 or TBool)
       -> [Variable]  -- ^ Variables that are in scope that the expression can use
       -> Int         -- ^ Size parameter
       -> Gen SSMExp
arbExp t vars 0 = oneof $ litGen : varGen
  where
    -- | Generator of SSMExp literals.
    litGen :: Gen SSMExp
    litGen = case t of
      TInt32 -> return  . Lit TInt32  . LInt32    =<< choose (0, 215)
      TInt64 -> return  . Lit TInt64  . LInt64  =<< choose (-55050, 55050)
      TUInt64 -> return . Lit TUInt64 . LUInt64 =<< choose (0, 65500)
      TBool  -> return  . Lit TBool   . LBool   =<< arbitrary

    -- | Generator that returns a randomly selected variable from the set of variables.
    varGen :: [Gen SSMExp]
    varGen = [ return (Var t' (getVarName n)) | (n,t') <- vars, t == t']

arbExp t vars n = case t of
  TBool -> oneof [ do e1 <- arbExp TInt32 vars (n `div` 2)
                      e2 <- arbExp TInt32 vars (n `div` 2)
                      return $ BOp t e1 e2 OLT
                 , do typ <- elements [TInt32, TBool]
                      e1 <- arbExp typ vars (n `div` 2)
                      e2 <- arbExp typ vars (n `div` 2)
                      return $ BOp TBool e1 e2 OEQ
                 ]
  t | t `elem` [TUInt8, TUInt64] -> do
      e1 <- arbExp t vars (n `div` 2)
      e2 <- arbExp t vars (n `div` 2)
      elements [ BOp t e1 e2 OPlus
               , BOp t e1 e2 OMinus
               , BOp t e1 e2 OTimes
               ]
  _ ->    frequency [ (1, do e <- arbExp t vars (n-1)
                             return $ UOp t e Neg
                      )
                    , (7, do e1 <- arbExp t vars (n `div` 2)
                             e2 <- arbExp t vars (n `div` 2)
                             elements [ BOp t e1 e2 OPlus
                                      , BOp t e1 e2 OMinus
                                      , BOp t e1 e2 OTimes
                                      ]
                      )
                    ]

{-********** Shrinking **********-}

{-****** Removing unused procedures ******-}

removeUnusedProcedures :: Program -> Program
removeUnusedProcedures p = case removeProcedure p (toremove' p) of
  Just p -> p
  Nothing -> p

usedInStm :: [Stm] -> Set.Set String
usedInStm [] = Set.empty
usedInStm (x:xs) = case x of
  If c thn els -> let s1 = usedInStm thn
                      s2 = usedInStm els
                      s3 = usedInStm xs
                  in Set.unions [s1,s2,s3]

  While c bdy -> let s1 = usedInStm bdy
                     s2 = usedInStm xs
                  in Set.union s1 s2

  Fork procs -> let s1 = Set.fromList $ map fst procs
                    s2 = usedInStm xs
                in Set.union s1 s2

  otherwise -> usedInStm xs

toremove' :: Program -> [String]
toremove' p = let s1 = Set.fromList $ Map.keys (funs p)
                  s2 = usedInStm $ body (fromJust (Map.lookup (main p) (funs p)))
                  s3 = Set.union s2 (Set.singleton (main p))
              in Set.toList $ s1 `Set.difference` s3

{-****** Removing declared references *****-}

shrinkRefs :: Program -> [Program]
shrinkRefs p = let procedures = Map.toList $ funs p
                  in concat $ for procedures $ \(n,pr) ->
  let ps = removeAllDeclaredRefs pr
  in map (\procedure -> p { funs = Map.insert n procedure (funs p)}) ps

-- | Given a procedure will return all successful transformations of the procedure
-- where a transformation is defined as the act of removing one of the declared
-- references from the program.
removeAllDeclaredRefs :: Procedure -> [Procedure]
removeAllDeclaredRefs p = let refs = allRefs p in removeRefs p refs

removeRefs :: Procedure -> [Ref] -> [Procedure]
removeRefs p refs =  map fromJust $ filter isJust $ for refs $ \ref -> do
    let initialrefs = filter (isReference . snd) $ arguments p
    body' <- removeRef' ref [] initialrefs (body p)
    return $ p { body = body' }
  where
    -- | Tries to remove a reference from a procedure. If successful it returns the new
    -- procedure, and if not it will return Nothing.
    removeRef' :: Ref          -- ^ Reference to remove
               -> [Variable]   -- ^ Variables that are no longer valid
               -> [Ref]        -- ^ References that are valid
               -> [Stm]        -- ^ Procedure body to transform
               -> Maybe [Stm]
    removeRef' _   _    _    []     = Just []
    removeRef' ref vars refs (x:xs) = case x of
      NewRef n t e -> let r = (getVarName n, t) in if r == ref
        then removeRef' ref vars refs xs
        else let stm = NewRef n t (deleteVars vars e)
             in (:) stm <$> removeRef' ref vars (r:refs) xs
      GetRef n t r -> if r == ref
        then removeRef' ref ((n,t):vars) refs xs
        else (:) x <$> removeRef' ref vars refs xs
      SetRef r e -> if r == ref
        then removeRef' ref vars refs xs
        else let stm = SetRef r (deleteVars vars e)
             in (:) stm <$> removeRef' ref vars refs xs
      SetLocal n t e -> if (n,t) `elem` vars
        then removeRef' ref vars refs xs
        else let stm = SetLocal n t (deleteVars vars e)
             in (:) stm <$> removeRef' ref vars refs xs
      If c thn els -> do
        let c' = deleteVars vars c
        thn'  <- removeRef' ref vars refs thn
        els'  <- removeRef' ref vars refs els
        let stm = If c' thn' els'
        (:) stm <$> removeRef' ref vars refs xs
      While c bdy -> do
        let c'  = deleteVars vars c
        bdy'   <- removeRef' ref vars refs bdy
        let stm = While c' bdy'
        (:) stm <$> removeRef' ref vars refs xs
      Skip -> (:) Skip <$> removeRef' ref vars refs xs
      After d r v -> if r == ref
        then removeRef' ref vars refs xs
        else let stm = After (deleteVars vars d) r (deleteVars vars v)
             in (:) stm <$> removeRef' ref vars refs xs
      Changed n t r -> if r == ref
        then           removeRef' ref ((n,t):vars) refs xs
        else (:) x <$> removeRef' ref vars refs xs
      Wait references -> let references' = filter ((/=) ref) references in
        if null references'
          then Nothing
          else (:) (Wait references') <$> removeRef' ref vars refs xs
      Fork procs -> case sequence (map removeRefFromFork procs) of
        Just procs' -> (:) (Fork procs') <$> removeRef' ref vars refs xs
        Nothing     -> Nothing
      where
        -- | Transform an expression into one that contains no invalid variables.
        deleteVars :: [Variable] -> SSMExp -> SSMExp
        deleteVars vars e = case e of
          Var t n        -> if n `elem` map (getVarName . fst) vars
                              then defaultValue t
                              else e
          BOp t e1 e2 op -> BOp t (deleteVars vars e1) (deleteVars vars e2) op
          UOp t e op     -> UOp t (deleteVars vars e) op
          otherwise      -> otherwise
          where
            defaultValue :: Type -> SSMExp
            defaultValue TInt32 = Lit TInt32 $ LInt32 1
            defaultValue TBool  = Lit TBool  $ LBool True

        -- | Try to remove a reference from a procedure call. This is done by replacing
        -- it with an arbitrary reference in scope of the same type.
        removeRefFromFork :: (String, [Either SSMExp Reference])
                          -> Maybe (String, [Either SSMExp Reference])
        removeRefFromFork (n, args) = do
          args' <- sequence $ for args $ \a -> case a of
                Left e  -> Just $ Left $ deleteVars vars e
                Right r -> if r == ref
                             then replaceRef $ snd r
                             else Just $ Right r
          return (n, args')
        
        -- | Try to replace a reference with a reference of the same type, if one
        -- exists. NOTE: The choice to take the head here is arbitrary. Discuss
        -- with Koen.
        replaceRef :: Type -> Maybe (Either SSMExp Reference)
        replaceRef t = case filter ((==) t . snd) refs of
            []    -> Nothing
            (x:_) -> Just $ Right x

-- | All declared references in a program (expect procedure arguments)
allRefs :: Procedure -> [(String, Type)]
allRefs p = refs $ body p
  where
    refs :: [Stm] -> [(String, Type)]
    refs xs = concat $ for xs $ \x -> case x of
      NewRef n t e -> [(getVarName n,t)]
      If _ thn els -> refs thn ++ refs els
      While _ bdy  -> refs bdy
      _            -> []

{-***** Removing entire procedures *****-}

-- | Return all mutations where one function were removed from the program. Never
-- tries to remove the main function.
shrinkSingleProcedures :: Program -> [Program]
shrinkSingleProcedures p = map fromJust $ filter isJust $ for toremove $ \fun ->
    let p' = p { funs = Map.delete fun (funs p) }
    in removeProcedure p' [fun]
  where
    toremove :: [String]
    toremove = delete (main p) (Map.keys (funs p))

-- | Return all mutations where half of functions were removed from the program. Never
-- tries to remove the main function.
shrinkHalfProcedures :: Program -> [Program]
shrinkHalfProcedures p = concat $ for [ removeProcedure p h1
                                      , removeProcedure p h2
                                      , removeProcedure p h3
                                      ] $ \mp ->
  if isJust mp
    then [fromJust mp]
    else []
  where
    toremove :: [String]
    toremove = delete (main p) (Map.keys (funs p))

    (h1,h2,h3) = let l       = length toremove `div` 3
                     (h1,r)  = mysplit l toremove
                     (h2,h3) = mysplit l r
                  in (h1,h2,h3)

    mysplit :: Int -> [a] -> ([a],[a])
    mysplit i xs = go i ([], xs)
      where
        go :: Int -> ([a],[a]) -> ([a],[a])
        go 0 (sx,ys)   = (reverse sx, ys)
        go i (sx,y:ys) = go (i-1) (y : sx, ys)

removeProcedure :: Program -> [String] -> Maybe Program
removeProcedure p procs = do
  funs' <- removeFromProcedures (Map.toList (funs p)) procs False
  return $ p { funs = Map.fromList funs' }

removeFromProcedures :: [(String,Procedure)]
                     -> [String]
                     -> Bool
                     -> Maybe [(String,Procedure)]
removeFromProcedures [] _ b             = if b then Just [] else Nothing
removeFromProcedures ((n,p):ps) procs b =
  if n `elem` procs
    then removeFromProcedures ps procs True
    else case remove p procs of
      Just p' -> do ps' <- removeFromProcedures ps procs (b || True)
                    return $ (n,p') : ps'
      Nothing -> do ps' <- removeFromProcedures ps procs (b || False)
                    return $ (n,p) : ps'

remove :: Procedure -> [String] -> Maybe Procedure
remove p funs = case newbody (body p, False) of
  (_, False)  -> Nothing
  (bdy, True) -> Just $ p { body = bdy }
  where
    newbody :: ([Stm], Bool) -> ([Stm], Bool)
    newbody ([], b)     = ([], b)
    newbody ((x:xs), b) = case x of
      If c thn els ->
        let (thn',b1) = newbody (thn, b)
            (els',b2) = newbody (els, b1)
            stm       = If c thn' els'
            (xs',b3)  = newbody (xs, b2)
        in (stm:xs', b3)

      While c bdy  ->
        let (bdy',b1) = newbody (bdy, b)
            stm       = While c bdy'
            (xs',b2)  = newbody (xs, b1)
        in (stm : xs', b2)
      
      Fork procs   -> do
        let procs' = removeFork procs funs
        case procs' of
          Just []      -> newbody (xs, b || True)
          Just procs'' -> let (xs',b1) = newbody (xs, b || True)
                          in (Fork procs'' : xs', b1)
          Nothing      -> let (xs',b1) = newbody (xs, b || False)
                          in (Fork procs : xs', b1)

      otherwise    ->
        let (xs',b1) = newbody (xs, b)
        in (otherwise : xs', b1)
    
    removeFork :: [(String, [Either SSMExp Reference])]
               -> [String]
               -> Maybe [(String, [Either SSMExp Reference])]
    removeFork procs funs = go procs funs False
      where
        go [] _ b                 = if b then Just [] else Nothing
        go (x@(n,args):xs) funs b = do
          if n `elem` funs
            then do go xs funs (b || True)
            else do xs' <- go xs funs (b || False)
                    return $ x : xs'
{-***** Shrinking/flattening if statements *****-}

shrinkIf :: Program -> [Program]
shrinkIf p = [ p { funs = Map.insert n proc' (funs p) } | (n,fun) <- Map.toList (funs p)
                                                        , proc' <- shrinkIfProcedure fun]

shrinkIfProcedure :: Procedure -> [Procedure]
shrinkIfProcedure p = let bodys = shrinkIfStm (emptyHughes, body p)
                      in for bodys $ \bdy -> p { body = bdy }

shrinkIfStm :: (Hughes Stm,[Stm]) -> [[Stm]]
shrinkIfStm (_,[])           = []
shrinkIfStm (front, (x:xs)) = case x of
  If c thn els -> let front' = fromHughes front
                      curr   = [front' ++ thn ++ xs, front' ++ els ++ xs]
                  in curr  ++ shrinkIfStm (snoc front x, xs)
  While c bdy -> let bdys  = shrinkIfStm (emptyHughes, bdy)
                     front' = fromHughes front
                     curr = [ front' ++ (While c bdy' : xs) | bdy' <- bdys]
                 in curr ++ shrinkIfStm (snoc front x, xs)
  _ -> shrinkIfStm (snoc front x, xs)

{-***** Shrinking wait instructions *****-}

shrinkWait :: Program -> [Program]
shrinkWait p = [ p { funs = Map.insert n proc' (funs p) } | (n,fun) <- Map.toList (funs p)
                                                          , proc' <- shrinkWaitProcedure fun]

shrinkWaitProcedure :: Procedure -> [Procedure]
shrinkWaitProcedure p = let bodys = shrinkWaitStm (emptyHughes, body p)
                        in for bodys $ \bdy -> p { body = bdy }

shrinkWaitStm :: (Hughes Stm, [Stm]) -> [[Stm]]
shrinkWaitStm (_, [])          = []
shrinkWaitStm (front, (x:xs)) = case x of
  While c bdy -> let bdys   = shrinkWaitStm (emptyHughes, bdy)
                     front' = fromHughes front
                     currs  = [ front' ++ (While c bdy' : xs) | bdy' <- bdys]
                 in currs ++ shrinkWaitStm (snoc front x, xs)
  
  Wait refs -> let sublists = filter (not . null) $ map (\r -> delete r refs) refs
                   front'   = fromHughes front
                   currs    = [ front' ++ (Wait l : xs) | l <- sublists]
               in currs ++ shrinkWaitStm (snoc front x, xs)
 
  _ -> shrinkWaitStm (snoc front x, xs)

{-***** Shrinking fork sizes *****-}

shrinkForks :: Program -> [Program]
shrinkForks p =  [ p { funs = Map.insert n f' (funs p) }
                 | (n,f) <- Map.toList (funs p), f' <- shrinkForksProcedure f]

shrinkForksProcedure :: Procedure -> [Procedure]
shrinkForksProcedure p = let bdys = shrinkForkStm (emptyHughes, body p)
                         in map (\bdy -> p { body = bdy } ) bdys

shrinkForkStm :: (Hughes Stm, [Stm]) -> [[Stm]]
shrinkForkStm (_, [])          = []
shrinkForkStm (front, (x:xs)) = case x of
  While c bdy  -> let bdys   = shrinkForkStm (emptyHughes, bdy)
                      front' = fromHughes front
                      curr   = [ (front' ++ (While c bdy' : xs)) | bdy' <- bdys]
                  in curr ++ shrinkForkStm (snoc front x, xs)

  Fork procs   ->
    let procss = filter (not . null) $ map (\f -> delete f procs) procs
        front' = fromHughes front
        curr   = [ front' ++ (Fork ps : xs) | ps <- procss]
    in curr ++ shrinkForkStm (snoc front x, xs)

  _ -> shrinkForkStm (snoc front x, xs)

{-***** Shrinking procedure arity *****-}

-- | For every procedure in the program, create n new programs where n is the arity
-- of that procedure. Each mutation has arity (n-1).
shrinkArity :: Program -> [Program]
shrinkArity p = 
        -- newproc = procedure with 1 less argument
  [ let newproc  = proc' { arguments = delete arg (arguments proc')
                         , body      = shrinkProcedureBody (name proc') a i (body proc')
                         }

        -- compute the rest of the procedure-bodies, where the arity in
        -- fork calls to the changed procedure are altered.
        withoutn = Map.toList $ Map.delete n (funs p)
        newfuns  = for withoutn $ \(f,pr) ->
                     (f, pr { body = removeArityFromCalls n i (body pr)})

        -- new funs map for the program
        funs'    = Map.insert n newproc $ Map.fromList newfuns

        -- construct the new program.
    in p { args = if (main p) == n then removeNth i (args p) else (args p)
         , funs = funs'
         }

  | (n,proc') <- Map.toList (funs p)
  , (arg@(a,t),i) <- zip (arguments proc') [0..]
  ]

-- | If we have removed the i:th argument from a procedure this function will
-- traverse a program and remove the i:th argument from any fork-point where
-- the mutated process is forked.
removeArityFromCalls :: String -> Int -> [Stm] -> [Stm]
removeArityFromCalls _ _ []     = []
removeArityFromCalls n i (x:xs) = case x of
  If c thn els -> If c
                    (removeArityFromCalls n i thn)
                    (removeArityFromCalls n i els) : removeArityFromCalls n i xs

  While c bdy -> While c (removeArityFromCalls n i bdy) : removeArityFromCalls n i xs

  Fork procs -> let procs' = for procs $ \(n',args) ->
                      if n' == n
                        then (n', sanitizeArgs $ removeNth i args)
                        else (n', sanitizeArgs args)
                in Fork procs' : removeArityFromCalls n i xs

  _ -> x : removeArityFromCalls n i xs
  where
    sanitizeArgs :: [Either SSMExp Reference] -> [Either SSMExp Reference]
    sanitizeArgs args = for args $ \a -> case a of
      Left e  -> Left $ mapExp e
      Right r -> Right r
    
    mapExp :: SSMExp -> SSMExp
    mapExp e = case e of
      Var t name     -> if name == n then defaultVal t else e
      Lit t l        -> Lit t l
      UOp t e op     -> UOp t (mapExp e) op
      BOp t e1 e2 op -> BOp t (mapExp e1) (mapExp e2) op

type ShrinkM a = Reader ShrinkSt a
data ShrinkSt = St { procname :: String    -- ^ Name of the procedure we are shrinking
                   , toremove :: String    -- ^ Name of argument to remove
                   , ordinal  :: Int       -- ^ Number the argument was
                   , badvars  :: [String]  -- ^ Variables that are not valid
                   }

class Named a where
  getName :: a -> String

instance Named Name where
  getName n = getVarName n

instance Named Ref where
  getName (r,t) = r

-- | This function rewrites a procedure body to account for a removed procedure argument.
shrinkProcedureBody :: String  -- ^ Name of the function we are rewriting.
                    -> String  -- ^ Name of the argument we removed.
                    -> Int     -- ^ Position of the argument in the argument list.
                    -> [Stm]   -- ^ Procedure body to rewrite.
                    -> [Stm]
shrinkProcedureBody f n i xs = runReader (shrinkArityStm xs) (St f n i [])
  where
     shrinkArityStm :: [Stm] -> ShrinkM [Stm]
     shrinkArityStm []     = return []
     shrinkArityStm (x:xs) = case x of
       NewRef n t e -> do
         e' <- alterExp e
         (:) (NewRef n t e') <$> shrinkArityStm xs

       GetRef n t r -> do
         b <- isOK r
         if b
           then (:) x <$> shrinkArityStm xs
           else tag n $ shrinkArityStm xs

       SetRef r e -> do
         b <- isOK r
         if b
           then do e' <- alterExp e
                   (:) (SetRef r e') <$> shrinkArityStm xs
           else shrinkArityStm xs

       SetLocal n t e -> do
         b <- isOK n
         if b
           then do e' <- alterExp e
                   (:) (SetLocal n t e') <$> shrinkArityStm xs
           else tag n $ shrinkArityStm xs

       If c thn els -> do
         c' <- alterExp c
         thn' <- shrinkArityStm thn
         els' <- shrinkArityStm els
         (:) (If c' thn' els') <$> shrinkArityStm xs

       While c bdy -> do
         c' <- alterExp c
         bdy' <- shrinkArityStm bdy
         (:) (While c' bdy') <$> shrinkArityStm xs

       Skip -> (:) Skip <$> shrinkArityStm xs

       After d r v -> do
         b <- isOK r
         if b
           then do d' <- alterExp d
                   v' <- alterExp v
                   (:) (After d' r v') <$> shrinkArityStm xs
           else shrinkArityStm xs

       Changed n t r -> do
         b <- isOK r
         if b
           then (:) (Changed n t r) <$> shrinkArityStm xs
           else tag n $ shrinkArityStm xs

       Wait references -> do
         references' <- filterM isOK references
         if null references'
           then shrinkArityStm xs
           else (:) (Wait references') <$> shrinkArityStm xs

       Fork procs -> do
         procs' <- (map fromJust . filter isJust) <$> mapM fork procs
         if null procs'
           then shrinkArityStm xs
           else (:) (Fork procs') <$> shrinkArityStm xs

     -- | Traverse an expression replace all usages of invalid variables with
     -- new default values.
     alterExp :: SSMExp -> ShrinkM SSMExp
     alterExp e = case e of
       Var t n        -> do bads <- asks badvars
                            tor  <- asks toremove
                            if n `elem` bads || n == tor
                              then return $ defaultVal t
                              else return $ Var t n
       UOp t e op     -> do
         e' <- alterExp e
         return $ UOp t e' op
       BOp t e1 e2 op -> do
         e1' <- alterExp e1
         e2' <- alterExp e2
         return $ BOp t e1' e2' op
       _              -> return e

     -- | Convert a fork-call to the same one but where the i:th argument has
     -- been removed. Which argument should be removed is known in the state
     -- of the monadic compuation.
     fork :: (String, [Either SSMExp Reference])
          -> ShrinkM (Maybe (String, [Either SSMExp Reference]))
     fork (n, args) = do
       n' <- asks procname
       args' <- alterArgs args
       case args' of
         Just newargs -> do
           if n' == n
             then asks ordinal >>= \i -> return $ Just (n, removeNth i newargs)
             else return $ Just $ (n, newargs)
         Nothing      -> return Nothing

     alterArgs :: [Either SSMExp Reference] -> ShrinkM (Maybe [Either SSMExp Reference])
     alterArgs xs = do
       xs' <- mapM alterArg xs
       if all isJust xs'
         then return $ Just $ map fromJust xs'
         else return Nothing

     alterArg :: Either SSMExp Reference -> ShrinkM (Maybe (Either SSMExp Reference))
     alterArg (Left e)  = (Just . Left) <$> alterExp e
     alterArg (Right r) = do
       tor <- asks toremove
       if fst r == tor
         then return Nothing
         else return $ Just $ Right r

     -- | Is the named entity okay to keep? The entity is either a variable of
     -- a reference.
     isOK :: Named a => a -> ShrinkM Bool
     isOK a = do
       bad  <- asks toremove
       bads <- asks badvars
       return $ not $ getName a `elem` bad:bads

     -- | Extend the environment with the name of a as a bad variable name.
     tag :: Named a => a -> ShrinkM b -> ShrinkM b
     tag a ma = local (\st -> st { badvars = getName a : badvars st }) ma

-- | Remove the n:th element from a list, with the first element being indexed as 0.
removeNth :: Show a => Int -> [a] -> [a]
removeNth 0 []     = error "can not remove from empty list"
removeNth 0 (_:xs) = xs
removeNth n (x:xs) = x : removeNth (n-1) xs

-- | Returns a default value for a type. Does not work for reference types.
defaultVal :: Type -> SSMExp
defaultVal TInt32 = Lit TInt32 $ LInt32 1
defaultVal TInt64 = Lit TInt64 $ LInt64 1
defaultVal TUInt64 = Lit TUInt64 $ LUInt64 1
defaultVal TBool  = Lit TBool $ LBool True

{-***** Remove unit-statements *****-}

-- | Return a list of new programs that are smaller by removing statements.
shrinkAllStmts :: Program -> [Program]
shrinkAllStmts p = [ p { funs = Map.insert n proc'' (funs p) }
                   | (n,proc') <- Map.toList (funs p)
                   , proc''    <- shrinkAllStmtsProcedure proc']

-- | Return a list of new procedures where the procedure is mutated by removing
-- statements.
shrinkAllStmtsProcedure :: Procedure -> [Procedure]
shrinkAllStmtsProcedure p = [ p { body = bdy } 
                            | bdy <- shrinkBody (emptyHughes, body p)]

-- | Shrink the statements of a procedure body. SetRef, SetLocal, If, While, Skip,
-- After, Wait and Fork can be 'safely' removed, where safely means that the rest of
-- the program is still type safe.
shrinkBody :: (Hughes Stm, [Stm]) -> [[Stm]]
shrinkBody (_, [])          = []
shrinkBody (front, (x:xs)) = case x of
  SetRef _ _     -> emitPartial : continue
  SetLocal _ _ _ -> emitPartial : continue
  If c thn els   -> let thns   = shrinkBody (emptyHughes, thn)
                        elss   = shrinkBody (emptyHughes, els)
                        front' = fromHughes front
                        currth = [ front' ++ (If c thn' els : xs) | thn' <- thns]
                        currel = [ front' ++ (If c thn els' : xs) | els' <- elss]
                    in currth ++ (currel ++ continue)
  While c bdy    -> let bdys   = shrinkBody (emptyHughes, bdy)
                        front' = fromHughes front
                        curr   = [ front' ++ (While c bdy' : xs) | bdy' <- bdys]
                    in curr ++ continue
  Skip           -> continue
  After _ _ _    -> emitPartial >> continue
  Wait _         -> emitPartial >> continue
  Fork _         -> emitPartial >> continue
  _              -> continue
  where
    emitPartial :: [Stm]
    emitPartial = fromHughes front ++ xs

    continue :: [[Stm]]
    continue = shrinkBody (snoc front x, xs)
