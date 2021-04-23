module LowGenerator where

import Data.List
import Data.Maybe
import qualified Data.Map as Map

import LowCore

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe ()
import Control.Monad.Reader

import System.IO.Unsafe

import CodeGen

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

instance Arbitrary Type where
    arbitrary = elements [TInt, TBool, Ref TInt, Ref TBool]

type Procedures = [(String, [(String, Type)], [Stm])]
type Variable = (Name, Type)
type Ref     = (String, Type)

instance Arbitrary Program where
  shrink p = shrinkRefs p ++ shrinkProcedures p
  arbitrary = do
    types <- arbitrary `suchThat` (not . null)
    let funs = [ ("fun" ++ show i, as) | (as,i) <- zip types [1..]]
    tab <- mfix $ \tab -> sequence
        [ do let (refs, vars) = partition (isReference . fst) $ zip as [1..]
             let inprefs      = [ ("ref" ++ show i        , t) | (t,i) <- refs]
             let inpvars      = [ (Fresh $ "var" ++ show i, t) | (t,i) <- vars]

             (body,_)        <- sized $ arbProc tab inpvars inprefs 0

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
      [ (1, do t         <- elements [TInt, TBool]
               e         <- choose (0,3) >>= arbExp t vars
               (name,c1) <- fresh c
               let rt     = mkReference t
               let stm    = NewRef name rt e
               let ref    = (getVarName name, rt)
               (rest, c2) <- arbProc funs vars (ref:refs) c1 (n-1)
               return (stm:rest, c2)
--               (:) stm <$> arbProc funs vars (ref:refs) c' (n-1) 
        )
      
      , (1, do cond      <- choose (0,3) >>= arbExp TBool vars
               (thn,c1)  <- arbProc funs vars refs c (n `div` 2)
               (els,c2)  <- arbProc funs vars refs c1 (n `div` 2)
               (rest,c3) <- arbProc funs vars refs c2 (n-1)
               let stm    = If cond thn els
               return (stm:rest, c3)
--               (:) stm <$> arbProc funs vars refs c (n-1) -- TODO make better size
        )
      
      --, (1, undefined {-while-}
      --  )

      , (1, do let forkable = elements funs `suchThat` canBeCalled refs
               tofork      <- listOf forkable `suchThat` (not . null)
               forks       <- mapM (applyFork vars refs) tofork
               let stm      = Fork forks
               (rest,c') <- arbProc funs vars refs c (n-1)
               return (stm:rest, c')
--               (:) stm <$> arbProc funs vars refs c (n-1)
        )
      ] ++

      (if null vars then [] else
      [ (1, do (name,t) <- elements vars
               e        <- choose (0,3) >>= arbExp t vars
               (rest,c') <- arbProc funs vars refs c (n-1)
               let stm   = SetLocal name t e
               return (stm:rest, c')
--               (:) stm <$> arbProc funs vars refs c (n-1)
        )]) ++

      (if null refs then [] else
      [ (1, do r@(_,t)   <- elements refs
               (name,c1) <- fresh c
               let t'    = dereference t
               (rest,c2) <- arbProc funs ((name, t'):vars) refs c1 (n-1)
               let stm   = GetRef name t' r
               return (stm:rest, c2)
--               (:) stm <$> arbProc funs ((name,t'):vars) refs c' (n-1)
        )
      
      , (1, do r@(_,t) <- elements refs
               e       <- choose (0,3) >>= arbExp (dereference t) vars
               (rest,c') <- arbProc funs vars refs c (n-1)
               let stm = SetRef r e
               return (stm:rest, c')
--               (:) stm <$> arbProc funs vars refs c (n-1)
        )
      
      , (1, do r@(_,t)  <- elements refs
               v        <- choose (0,3) >>= arbExp (dereference t) vars
               delay'   <- choose (0,3) >>= arbExp TInt vars
               (rest,c') <- arbProc funs vars refs c (n-1)
               let delay = delay' * delay' + 1 -- hack to make it non negative and non zero
               let stm   = After delay r v
               return (stm:rest, c')
--               (:) stm <$> arbProc funs vars refs c (n-1)
        )
      
      , (1, do r         <- elements refs
               (name,c1) <- fresh c
               (rest,c2) <- arbProc funs ((name, TBool):vars) refs c1 (n-1)
               let stm    = Changed name TBool r
               return (stm:rest, c2)
--               (:) stm <$> arbProc funs ((name,TBool):vars) refs c' (n-1)
        )
      
      , (1, do refs'  <- sublistOf refs `suchThat` (not . null)
               (rest,c') <- arbProc funs vars refs c (n-1)
               let stm = Wait refs'
               return (stm:rest, c')
--               (:) stm <$> arbProc funs vars refs c (n-1)
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
arbExp :: Type        -- ^ Type of expression to generate (oneof TInt or TBool)
       -> [Variable]  -- ^ Variables that are in scope that the expression can use
       -> Int         -- ^ Size parameter
       -> Gen SSMExp
arbExp t vars 0 = oneof $ litGen : varGen
  where
    -- | Generator of SSMExp literals.
    litGen :: Gen SSMExp
    litGen = case t of
      TInt  -> return . Lit TInt  . LInt =<< arbitrary
      TBool -> return . Lit TBool . LBool =<< arbitrary
    
    -- | Generator that returns a randomly selected variable from the set of variables.
    varGen :: [Gen SSMExp]
    varGen = [ return (Var t' (getVarName n)) | (n,t') <- vars, t == t']

arbExp t vars n = case t of
  TInt -> frequency [ (1, do e <- arbExp t vars (n-1)
                             return $ negate e
                      )
                    , (7, do e1 <- arbExp t vars (n `div` 2)
                             e2 <- arbExp t vars (n `div` 2)
                             elements [ e1 + e2
                                      , e1 - e2
                                      , e1 * e2
                                      ]
                      )
                    ]
  TBool -> oneof [ do e1 <- arbExp TInt vars (n `div` 2)
                      e2 <- arbExp TInt vars (n `div` 2)
                      return $ BOp t e1 e2 OLT
                 , do typ <- elements [TInt, TBool]
                      e1 <- arbExp typ vars (n `div` 2)
                      e2 <- arbExp typ vars (n `div` 2)
                      return $ BOp TBool e1 e2 OEQ
                 ]

{-********** Shrinking **********-}

{-****** Removing declared references *****-}

testprogram1 :: Program
testprogram1 = Program "fun1" [] $ Map.fromList [("fun1",
    Procedure "fun1" [] [If (BOp TBool (UOp TInt (Lit TInt (LInt 1)) Neg) (UOp TInt (Lit TInt (LInt 2)) Neg) OLT)
                           [ Fork [("fun1",[])]]
                           [ NewRef (Fresh "v0") (Ref TBool) (BOp TBool (BOp TInt (Lit TInt (LInt 2)) (Lit TInt (LInt 1)) OPlus) (BOp TInt (Lit TInt (LInt 1)) (Lit TInt (LInt 0)) OPlus) OEQ)
                           --, NewRef (Fresh "v1") (Ref TInt) (Lit TInt (LInt 5))
                           , Fork [("fun1",[]),("fun1",[]),("fun1",[])]]])]

testprogram2 :: Program
testprogram2 = Program "fun1" [] $ Map.fromList [
  ("fun1", Procedure "fun1" [] [ NewRef (Fresh "v0") (Ref TInt) (Lit TInt $ LInt 20)
                               , If (Lit TBool $ LBool False)
                                   [ Fork [("fun1",[])]]
                                   [ NewRef (Fresh "v1") (Ref TInt) (Lit TInt $ LInt 5)
                               , Wait [("v1", Ref TInt)]]
                               , Fork [("fun2",[])]
                               , After (Lit TInt $ LInt 1) ("v0", Ref TInt) (Lit TInt $ LInt 10)
                               ]
  ),
  ("fun2", Procedure "fun2" [] [NewRef (Fresh "v0") (Ref TInt) (Lit TInt $ LInt 20)])
  ]

shrinkRefs :: Program -> [Program]
shrinkRefs p = let procedures = Map.toList $ funs p
                  in concat $ flip map procedures $ \(n,pr) ->
  let ps = removeAllDeclaredRefs pr
  in map (\procedure -> p { funs = Map.insert n procedure (funs p)}) ps

-- | Given a procedure will return all successful transformations of the procedure
-- where a transformation is defined as the act of removing one of the declared
-- references from the program.
removeAllDeclaredRefs :: Procedure -> [Procedure]
removeAllDeclaredRefs p = let refs = allRefs p in removeRefs p refs

removeRefs :: Procedure -> [Ref] -> [Procedure]
removeRefs p refs =  map fromJust $ filter isJust $ flip map refs $ \ref -> do
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
            defaultValue TInt  = Lit TInt $ LInt 1
            defaultValue TBool = Lit TBool $ LBool True

        -- | Try to remove a reference from a procedure call. This is done by replacing
        -- it with an arbitrary reference in scope of the same type.
        removeRefFromFork :: (String, [Either SSMExp Reference])
                          -> Maybe (String, [Either SSMExp Reference])
        removeRefFromFork (n, args) = do
          args' <- sequence $ flip map args $ \a -> case a of
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
    refs xs = concat $ flip map xs $ \x -> case x of
      NewRef n t e -> [(getVarName n,t)]
      If _ thn els -> refs thn ++ refs els
      While _ bdy  -> refs bdy
      _            -> []

{-***** Removing entire procedures *****-}

shrinkProcedures :: Program -> [Program]
shrinkProcedures p = map fromJust $ filter isJust $ flip map toremove $ \fun ->
    let p' = p { funs = Map.delete fun (funs p) }
    in removeProcedure p' fun
  where
    toremove :: [String]
    toremove = delete (main p) (Map.keys (funs p))

removeProcedure :: Program -> String -> Maybe Program
removeProcedure p fun = do
  procedures <- sequence [ do pro' <- remove pro fun
                              return (n,pro')
                         | (n,pro) <- Map.toList (funs p)]
  return $ p { funs = Map.fromList procedures}

remove :: Procedure -> String -> Maybe Procedure
remove p fun = let body' = newbody (body p) in
               if body' /= (body p)
                 then Just $ p { body = newbody (body p) }
                 else Nothing
  where
    newbody :: [Stm] -> [Stm]
    newbody []     = []
    newbody (x:xs) = case x of
      If c thn els ->
        let stm = If c (newbody thn) (newbody els)
        in stm : newbody xs

      While c bdy  ->
        let stm = While c $ newbody bdy
        in stm : newbody xs
      
      Fork procs   -> let procs' = removeFork procs fun in
        if null procs'
          then newbody xs
          else Fork procs' : newbody xs
      
      otherwise    -> otherwise : newbody xs
    
    removeFork :: [(String, [Either SSMExp Reference])]
               -> String
               -> [(String, [Either SSMExp Reference])]
    removeFork procs fun = filter ((/=) fun . fst) procs