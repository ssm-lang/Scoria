{-| This module exposes a pretty printer of programs. -}
module SSM.Pretty.Syntax ( prettyProgram ) where

import qualified Data.Map as Map
import Data.List

import Control.Monad.Reader
    ( ReaderT(runReaderT), MonadReader(local, ask) )
import Control.Monad.Writer
    ( execWriter, MonadWriter(tell), Writer )

import SSM.Core

import SSM.Util.HughesList ( fromHughes, toHughes, Hughes )

type PP a = ReaderT Int                    -- current level of indentation
              (Writer (Hughes String)) a   -- output

emit :: String -> PP ()
emit str = do
    ind <- ask
    tell $ toHughes [replicate ind ' ' ++ str]

indent :: PP () -> PP ()
indent = local (+2)

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
    emit "initial ready-queue content:"
    mapM_ (indent . emit . prettyQueueContent) (initialQueueContent p) 
    emit ""
    emit "global variables:"
    mapM_ prettyPeripheralDeclarations (peripherals p)
    emit ""
    intercalateM (emit "") $ map prettyProcedure (Map.elems (funs p))
    return ()

prettyQueueContent :: QueueContent -> String
prettyQueueContent (SSMProcedure id args) = prettyApp (id, args)
prettyQueueContent (Handler h           ) = case h of
    Output variant ref -> case variant of
        LED id -> prettyApp
            ( Ident "led_output_handler" Nothing
            , [Right ref, Left $ Lit TUInt8 $ LUInt8 id]
            )
        BLE bh -> case bh of
            Broadcast        -> prettyApp
                ( Ident "broadcast_output_handler" Nothing
                , [Right ref]
                )
            BroadcastControl -> prettyApp
                ( Ident "broadcast_control_output_handler" Nothing
                , [Right ref]
                )
            ScanControl    -> prettyApp
                ( Ident "scan_control_output_handler" Nothing
                , [Right ref]
                )

prettyReferenceDecls :: [Reference] -> PP ()
prettyReferenceDecls xs = flip mapM_ xs $ \ref ->
    indent $ emit $ concat [prettyType (refType ref), " ", refName ref]

prettyPeripheralDeclarations :: Peripheral -> PP ()
prettyPeripheralDeclarations (Peripheral p) =
    prettyReferenceDecls $ declaredReferences p

prettyProcedure :: Procedure -> PP ()
prettyProcedure p = do
    let params = map prettyParam (arguments p)
    let header = concat [identName $ name p, "(", intercalate ", " params, ") {"]
    emit header
    indent $ mapM_ prettyStm (body p)
    emit "}"
  where
      prettyParam :: (Ident, Type) -> String
      prettyParam (n,t) = concat [prettyType t, " ", identName n]

prettyStm :: Stm -> PP ()
prettyStm stm = case stm of
    NewRef n t e   -> emit $ concat [ prettyType t
                                    , " *"
                                    , identName n
                                    , " = var "
                                    , prettySSMExp e
                                    ]
    SetRef r e     -> emit $ concat [ refName r
                                    , " = "
                                    , prettySSMExp e
                                    ]
    SetLocal n t e -> emit $ concat [ identName n
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
        emit "}"
    Skip           -> return ()

    After d r v    -> emit $ concat [ "after "
                                    , prettySSMExp d
                                    , " then "
                                    , refName r
                                    , " = "
                                    , prettySSMExp v
                                    ]

    Wait refs      -> emit $ concat [ "wait ["
                                    , intercalate ", " (map refName refs)
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

prettyApp :: (Ident, [Either SSMExp Reference]) -> String
prettyApp (n, args) = concat [ identName n
                             , "("
                             , intercalate ", " (map printarg args)
                             , ")"
                             ]
  where
      printarg :: Either SSMExp Reference -> String
      printarg = either prettySSMExp refName

prettyType :: Type -> String
prettyType t = case t of
    TInt32  -> "int"
    TUInt8  -> "uint8"
    TUInt32 -> "uint32"
    TInt64  -> "int64"
    TUInt64 -> "uint64"
    TBool   -> "bool"
    TEvent  -> "event"
    Ref t   -> prettyType t ++ "*"

prettyLit :: SSMLit -> String
prettyLit l = case l of
    LInt32 i  -> show i
    LUInt8 i  -> show i
    LUInt32 i -> show i
    LInt64 i  -> show i
    LUInt64 i -> show i
    LBool b   -> show b
    LEvent    -> show ()

prettySSMExp :: SSMExp -> String
prettySSMExp e = case e of
    Var t n         -> identName n
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
    Not -> concat ["!", prettySSMExp e]

prettyUnaryOpR :: UnaryOpR -> Reference -> String
prettyUnaryOpR op r = case op of
    Changed -> '@' : refName r
    Deref   -> '*' : refName r

prettyBinop :: BinOp -> String
prettyBinop op = case op of
    OPlus   -> "+"
    OMinus  -> "-"
    OTimes  -> "*"
    ODiv    -> "/"
    ORem    -> "%"
    OMin    -> "`min`"
    OMax    -> "`max`"
    OLT     -> "<"
    OEQ     -> "=="
    OAnd    -> "&&"
    OOr     -> "||"
    OLShift -> "<<"
    ORShift -> ">>"
    OBAnd   -> "&"
    OBOr    -> "|"
    OBXor   -> "xor"
