module LowPretty where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import qualified Data.Map as Map
import HughesList

import qualified Core as C
import LowCore

type PP a = ReaderT Int               -- current level of indentation
              (Writer (Hughes String)) a   -- output

emit :: String -> PP ()
emit str = do
    ind <- ask
    tell $ toHughes [replicate ind ' ' ++ str]

indent :: PP () -> PP ()
indent pp = local (+2) pp

intercalateM :: Monad m => m a -> [m a] -> m [a]
intercalateM _ [] = return []
intercalateM _ [x] = x >>= (return . flip (:) [])
intercalateM ma (x:y:xs) = do
    x' <- x
    y' <- ma
    xs' <- intercalateM ma (y:xs)
    return $ x' : y' : xs'


prettyProgram :: Program -> String
prettyProgram ssm = let wr = runReaderT (prettyProgram' ssm) 0
                        h  = execWriter wr
                    in unlines $ fromHughes h

prettyProgram' :: Program -> PP ()
prettyProgram' p = do
    emit "entrypoint:"
    indent $ emit $ prettyApp (main p, args p)
--    emit "main() {"
--    indent $ emit $ prettyApp (main p, args p)
--    emit "}"
    emit ""
    intercalateM (emit "") $ map prettyProcedure (Map.elems (funs p))
    return ()

prettyProcedure :: Procedure -> PP ()
prettyProcedure p = do
    let params = map prettyParam (arguments p)
    let header = concat [name p, "(", intercalate ", " params, ") {"]
    emit header
    indent $ mapM_ prettyStm (body p)
    emit "}"
  where
      prettyParam :: (String, Type) -> String
      prettyParam (n,t) = concat [prettyType t, " ", n]

prettyStm :: Stm -> PP ()
prettyStm stm = case stm of
    NewRef n t e   -> emit $ concat [ prettyType t
                                    , " "
                                    , getVarName n
                                    , " = var "
                                    , prettySSMExp e
                                    ]
    GetRef n t r   -> emit $ concat [ prettyType t
                                    , " "
                                    , getVarName n
                                    , " = *"
                                    , fst r
                                    ]
    SetRef r e     -> emit $ concat [ fst r
                                    , " = "
                                    , prettySSMExp e
                                    ]
    SetLocal n t e -> emit $ concat [ getVarName n
                                    , " = "
                                    , prettySSMExp e
                                    ]
    If c thn els   -> do
        emit $ concat ["if(", prettySSMExp c, ") {"]
        indent $ mapM_ prettyStm thn
        emit "} else {"
        indent $ mapM_ prettyStm els
        emit "}"
    While c bdy    -> do
        emit $ concat ["while(", prettySSMExp c, ") {"]
        indent $ mapM_ prettyStm bdy
        emit $ "}"
    Skip           -> return ()
    After d r v    -> emit $ concat [ "after "
                                    , prettySSMExp d
                                    , " then "
                                    , fst r
                                    , " = "
                                    , prettySSMExp v
                                    ]
    Changed n t r  -> emit $ concat [ prettyType t
                                    , " "
                                    , getVarName n
                                    , " = @"
                                    , fst r]
    Wait refs      -> emit $ concat [ "wait ["
                                    , intercalate ", " (map fst refs)
                                    , "]"
                                    ]
    Fork procs     -> do
        let procs' = map prettyApp procs
        emit $ concat ["fork [", intercalate ", " procs', "]"]

prettyApp :: (String, [Either SSMExp Reference]) -> String
prettyApp (n, args) = concat [ n
                             , "("
                             , intercalate ", " (map printarg args)
                             , ")"
                             ]
  where
      printarg :: Either SSMExp Reference -> String
      printarg = either prettySSMExp fst

prettyType :: Type -> String
prettyType t = case t of
    TInt    -> "int"
    TUInt8  -> "uint8"
    TInt64  -> "int64"
    TUInt64 -> "uint64"
    TBool   -> "bool"
    Ref t   -> "*" ++ prettyType t

prettyLit :: SSMLit -> String
prettyLit l = case l of
    LInt i    -> show i
    LUInt8 i  -> show i
    LInt64 i  -> show i
    LUInt64 i -> show i
    LBool b   -> show b

prettySSMExp :: SSMExp -> String
prettySSMExp e = case e of
    Var t n         -> n
    Lit t l         -> prettyLit l
    UOp t e Neg     -> concat ["(", prettySSMExp e, ")"]
    BOp t e1 e2 bop -> concat [ "("
                              , prettySSMExp e1
                              , " "
                              , prettyBinop bop
                              , " "
                              , prettySSMExp e2
                              , ")"
                              ]

prettyBinop :: BinOp -> String
prettyBinop op = case op of
    OPlus  -> "+"
    OMinus -> "-"
    OTimes -> "*"
    OLT    -> "<"
    OEQ    -> "=="
{-

-- | SSM expressions
data SSMExp = Var Type String               -- ^ Variables
            | Lit Type SSMLit               -- ^ Literals
            | UOp Type SSMExp UnaryOp       -- ^ Unary operators
            | BOp Type SSMExp SSMExp BinOp  -- ^ Binary operators


printProcedure :: [SSMStm] -> PP ()
printProcedure [] = return ()
printProcedure ssm@(Procedure n:_) = do
    (wr,_) <- get
    if n `elem` wr
        then return ()
        else do modify $ \(na,o) -> (n:na, o)
                f <- censor (const []) $ snd <$> listen (local (const 0) (toString' ssm))
                modify $ \(na,o) -> (na,o ++ [unlines f])
  where
      toString' :: [SSMStm] -> PP ()
      toString' = mapM_ $ \ssm -> case ssm of
          (NewRef n e)    -> do
              let name = getVarName n
              emit $ name ++ " = NewRef " ++ show e
          (SetRef (r,_) e)    -> do
              emit $ "SetRef " ++ r ++ " = " ++ show e
          (SetLocal e v)  -> do
              emit $ "SetLocal " ++ show e ++ " = " ++ show v
          (GetRef n (r,t))      -> do
              let name = getVarName n
              emit $ name ++ " = GetRef " ++ r
          (If c thn (Just els))  -> do
              emit $ "If " ++ show c
              emit   "Then"
              indent $ toString' $ runSSM thn
              emit   "Else"
              indent $ toString' $ runSSM els
          (If c thn Nothing)  -> do
              emit $ "If " ++ show c
              emit   "Then"
              indent $ toString' $ runSSM thn
          (While c bdy)   -> do
              emit $ "While " ++ show c
              indent $ toString' $ runSSM bdy
          (After e (r,_) v)   -> do
              emit $ "After " ++ show e ++ " " ++ r ++ " = " ++ show v
          (Changed n (r,_))   -> do
              let name = getVarName n
              emit $ name ++ " = @" ++ r
          (Wait vars)     -> do
              emit $ "Wait [" ++ intercalate ", " (map fst vars) ++ "]"
          (Fork procs)    -> do
              i <- ask
              let indent = replicate (i + length "fork ") ' '
              let sep = '\n' : indent
              let calls = map printProcedureCall procs
              emit $ "Fork [ " ++ sep ++ "  " ++ intercalate (sep ++ ", ") calls ++ sep ++ "]"
              mapM_ (printProcedure . runSSM) procs
          (Procedure n) -> do
              emit $ "Procedure " ++ n
          (Argument n name (Left e))  -> do
              emit $ "Argument " ++ name ++ " = " ++ show e
          (Argument n name (Right r))  -> do
              emit $ "Argument &" ++ name ++ " = " ++ fst r
          (Result n)    -> do
              emit $ "Result"
        
      printProcedureCall :: SSM () -> String
      printProcedureCall ssm = name ++ "(" ++ intercalate ", " args ++ ")"
          where
              stmts = runSSM ssm

              name :: String
              name = getProcedureName (head stmts)

              getArgs :: [SSMStm] -> [String]
              getArgs (Argument _ _ (Left e):xs)      = show e : getArgs xs
              getArgs (Argument _ _ (Right (n,_)):xs) = n      : getArgs xs
              getArgs _                               = []

              args :: [String]
              args = getArgs (tail stmts)

      indent :: PP () -> PP ()
      indent pp = local (+2) pp

      emit :: String -> PP ()
      emit str = do
          ind <- ask
          tell [replicate ind ' ' ++ str]

-}