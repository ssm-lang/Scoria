{-| This module exposes a pretty printer of programs. -}
module SSM.Pretty.Syntax ( prettyProgram ) where

import qualified Data.Map as Map
import Data.List

import Control.Monad.Reader
    ( ReaderT(runReaderT), MonadReader(local, ask) )
import Control.Monad.Writer
    ( execWriter, MonadWriter(tell), Writer )

import SSM.Core.Syntax
    ( getVarName,
      BinOp(..),
      Reference,
      SSMExp(..),
      SSMLit(..),
      SSMTime(..),
      Type(..),
      UnaryOpE(..),
      UnaryOpR(..),
      Procedure(body, name, arguments),
      Program(funs, args, entry),
      Stm(..) )
import SSM.Util.HughesList ( fromHughes, toHughes, Hughes )

type PP a = ReaderT Int                    -- current level of indentation
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

{- | Pretty print a program. There is no control of line width currently.
If your program contains many nested if's or something, they will be turned
into quite wide statements. -}
prettyProgram :: Program -> String
prettyProgram ssm = let wr = runReaderT (prettyProgram' ssm) 0
                        h  = execWriter wr
                    in unlines $ fromHughes h

prettyProgram' :: Program -> PP ()
prettyProgram' p = do
    emit "entrypoint:"
    indent $ emit $ prettyApp (entry p, args p)
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
    After (SSMTime dur units) r v -> emit $ concat [ "after ("
                                                   , prettySSMExp dur
                                                   , show units
                                                   , ") then "
                                                   , fst r
                                                   , " = "
                                                   , prettySSMExp v
                                                   ]
    Wait refs      -> emit $ concat [ "wait ["
                                    , intercalate ", " (map fst refs)
                                    , "]"
                                    ]
    Fork procs     -> do
        let procs' = map prettyApp procs
        sep <- separator
        ind <- ask
        emit $ concat ["fork [ ", intercalate sep procs', "\n", replicate (ind + 5) ' ', "]"]
    where
        separator :: PP String
        separator = do
            ind <- ask
            return $ '\n' : replicate (ind + 5) ' ' ++ ", "

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
    TInt32  -> "int"
    TUInt8  -> "uint8"
    TInt64  -> "int64"
    TUInt64 -> "uint64"
    TBool   -> "bool"
    TEvent  -> "event"
    Ref t   -> "*" ++ prettyType t

prettyLit :: SSMLit -> String
prettyLit l = case l of
    LInt32 i  -> show i
    LUInt8 i  -> show i
    LInt64 i  -> show i
    LUInt64 i -> show i
    LBool b   -> show b
    LEvent    -> show ()

prettySSMExp :: SSMExp -> String
prettySSMExp e = case e of
    Var t n         -> n
    Lit t l         -> prettyLit l
    UOpE t e op     -> prettyUnaryOpE op e
    UOpR t r op     -> prettyUnaryOpR op r
    BOp t e1 e2 bop -> concat [ "("
                              , prettySSMExp e1
                              , " "
                              , prettyBinop bop
                              , " "
                              , prettySSMExp e2
                              , ")"
                              ]

prettyUnaryOpE :: UnaryOpE -> SSMExp -> String
prettyUnaryOpE op e = case op of
    Neg -> concat ["(-", prettySSMExp e, ")"] 

prettyUnaryOpR :: UnaryOpR -> Reference -> String
prettyUnaryOpR op r = case op of
    Changed -> '@' : fst r

prettyBinop :: BinOp -> String
prettyBinop op = case op of
    OPlus  -> "+"
    OMinus -> "-"
    OTimes -> "*"
    OLT    -> "<"
    OEQ    -> "=="
