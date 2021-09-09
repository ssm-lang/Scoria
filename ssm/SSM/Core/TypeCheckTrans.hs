module SSM.Core.TypeCheckTrans where

import SSM.Core.Syntax
    ( Program(..),
      Procedure(..),
      Stm(..),
      SSMLit(..),
      SSMExp(..),
      Reference(..),
      Type(..),
      Ident (..),
      SSMTime (..),
      BinOp(..),
      UnaryOpE(..),
      UnaryOpR(..),
      SSMTimeUnit (..) )
import qualified Data.Map as Map
import System.IO ()

import Control.Monad.Reader  -- Reader for context
import Control.Monad.Except -- Except for error handling (synonymous with manually throwing around Either e a, as you are doing now)

data Context = Context { procedures :: Map.Map Ident [(Ident, Type)]  -- ^ types of procedures
                       , scopes     :: [Map.Map Ident Type]  -- ^ types of variables (head of this list is the youngest scope)
                       }

type TC a = ReaderT Context (Except TypeError) a

-- | Type to record and report the type error
data TypeError = UnboundVariable Ident   -- ^ variable @Ident@ is not ins cope
               | TypeError Type Type  -- ^ The expression failed to type check, found t1 when t2 was expected
               -- more variants as needed

-- | Typechecks a program
-- We will first check whether the provided arguments for the entrypoint procedure
-- have the correct types
-- And then, we will check each procedure stored in the [funs] map
typeCheckProgram :: Program -> TC ()
typeCheckProgram Program {entry=e, funs=fs, globalReferences=gRefs} = do 
    params <- lookupProcedure e
    typeCheckProcs fs env'
    where
        env = enterProcs (return emptyContext) fs
        env' = foldl enterArg env gRefs

-- | Typechecks a procedure
typeCheckProcedure ::TC Context -> Procedure -> TC ()
typeCheckProcedure env Procedure {name=n, arguments=args, body=b} = do
    env <- typeCheckStmLst b newEnv
    return ()
    where newEnv = foldl enterArg env args

-- | Checks the functions in a String-Procedure map all have the corret type
typeCheckProcs :: Map.Map Ident Procedure -> TC Context -> TC ()
typeCheckProcs funs env = mapM_ (typeCheckProcedure env) (Map.elems funs)

-- | Typechecks an expression, meanwhile figuring out the type of the expression
typeCheckExp :: SSMExp -> TC a -> TC Type
typeCheckExp (Var ty ident) tca = do
    t <- lookupVar ident
    assertType ty t
    return ty
typeCheckExp (Lit ty lit) tca = do 
    actualTy <- typeCheckLit lit 
    assertType actualTy ty
    return ty
typeCheckExp (UOpE ty expr op) tca = do
    actualTy <- typeCheckExp expr tca
    assertType actualTy ty
    return ty
typeCheckExp (UOpR ty ref op) tca = do
    actualTy <- typeCheckRef tca ref
    assertType actualTy ty
    return ty
typeCheckExp (BOp ty e1 e2 op) tca = do
    actualTy1 <- typeCheckExp e1 tca
    actualTy2 <- typeCheckExp e2 tca
    assertType actualTy1 actualTy2
    assertType actualTy1 ty
    return ty

-- | Typechecks a reference
typeCheckRef :: TC a -> Reference -> TC Type
typeCheckRef tca (Dynamic (ident, ty)) = do
    actualTy <- lookupVar ident
    assertType actualTy ty
    return ty
typeCheckRef tca (Static (ident, ty)) = do
    actualTy <- lookupVar ident
    assertType actualTy ty
    return ty

-- | Typechecks a statement, meanwhile generating a new environment 
-- with the definitions in the statement
typeCheckStm :: Stm -> TC Context -> TC Context
typeCheckStm Skip tca = tca
typeCheckStm (After time ref exp2) tca = do
    refTy <- typeCheckRef tca ref
    expTy <- typeCheckExp exp2 tca
    assertType expTy refTy
    tca
typeCheckStm (Wait refs) tca = do
    mapM_ (typeCheckRef tca) refs
    tca
typeCheckStm (While expr stms) tca = do
    exprTy <- typeCheckExp expr tca
    assertType exprTy TBool
    tca
typeCheckStm (If expr stms1 stms2) tca = do
    exprTy <- typeCheckExp expr tca
    assertType exprTy TBool 
    typeCheckStmLst (stms1 ++ stms2) tca
typeCheckStm (SetRef ref expr) tca = do
    refTy <- typeCheckRef tca ref
    expTy <- typeCheckExp expr tca
    assertType expTy refTy
    tca
typeCheckStm (SetLocal name ty expr) tca = do
    actualTy <- typeCheckExp expr tca
    assertType actualTy ty
    local (enterVar name ty) tca
typeCheckStm (NewRef ident ty expr) tca = do 
    actualTy <- typeCheckExp expr tca
    assertType actualTy ty
    local (enterVar ident ty) tca
typeCheckStm (Fork procs) env = do
    typeCheckForkProcs procs env
    env

-- | Typechecks a list of statements
typeCheckStmLst :: [Stm] -> TC Context -> TC Context
typeCheckStmLst [] tca = tca
typeCheckStmLst (h:t) tca = do
    newTC <- typeCheckStm h tca
    typeCheckStmLst t $ return newTC

-- | Checks whether an argument has the correct type
typeCheckArg :: TC Context -> (Either SSMExp Reference, (Ident, Type)) -> TC ()
typeCheckArg env (Left expr, (name, ty)) = do
    expTy <- typeCheckExp expr env
    assertType expTy ty
typeCheckArg env (Right (Dynamic (ident, ty1)), (name, ty2)) = assertType ty2 ty1
typeCheckArg env (Right (Static (ident, ty1)), (name, ty2)) = assertType ty2 ty1

-- | Checks whether a list of arguments all have the correct type
typeCheckArgs :: [Either SSMExp Reference] -> [(Ident, Type)] -> TC Context -> TC ()
typeCheckArgs args params env = mapM_ (typeCheckArg env) (zip args params)

-- | Typechecks forked procedures
typeCheckForkProcs :: [(Ident, [Either SSMExp Reference])] -> TC Context -> TC ()
typeCheckForkProcs procs env =
    mapM_ (typeCheckForkProc env) procs

-- | Typechecks one forked procedure
typeCheckForkProc :: TC Context -> (Ident, [Either SSMExp Reference]) -> TC ()
typeCheckForkProc env (name, args) = do
    params <- lookupProcedure name
    typeCheckArgs args params env

{- | Look up the type of a variable. As our language has scopes (e.g if-branches & while loops), we must
dig through all the scopes to search for a type for the variable. We allow shadowing variables, so we start
looking for the type in the youngest scope and work our way towards the oldest. If no scope contains the variable,
it is clearly unbound and we encountered an error. -}
lookupVar :: Ident -> TC Type
lookupVar id = do
  e <- ask
  lookupVar' id (scopes e)
  where
    lookupVar' :: Ident -> [Map.Map Ident Type] -> TC Type
    lookupVar' id [] = throwError $ UnboundVariable id
    lookupVar' id (c:cs) = case Map.lookup id c of
      Just t -> return t
      Nothing -> lookupVar' id cs

-- | Look up the parameter types of a procedure.
lookupProcedure :: Ident -> TC [(Ident, Type)]
lookupProcedure id = do
  e <- ask
  case Map.lookup id (procedures e) of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable id

-- | Insert an identifier and its type into the map
insertVarToMap :: Ident -> Type -> [Map.Map Ident Type] -> [Map.Map Ident Type]
insertVarToMap ident t (c:cs) = Map.insert ident t c : cs
insertVarToMap ident t [] = [Map.insert ident t Map.empty]

-- | enter procedure definition into our environment
enterProc' :: Procedure -> Context -> Context
enterProc' Procedure {name=n, arguments=args, body=_} Context {procedures=p, scopes=s} =
    Context {procedures = Map.insert n args p, scopes = s}

enterProc :: TC Context -> Procedure -> TC Context
enterProc env p = local (enterProc' p) env

-- | enter parameter definitions in the Map `funs` into our environment
enterProcs :: TC Context -> Map.Map Ident Procedure -> TC Context
enterProcs env funs = foldl enterProc env (Map.elems funs)

-- | enter an argument and its type into our environment
enterArg :: TC Context -> (Ident, Type) -> TC Context
enterArg env (name, ty) = local (enterVar name ty) env

-- | Add a variable to our context
enterVar :: Ident -> Type -> Context -> Context
enterVar ident t Context {procedures=proc_map, scopes=_scopes} =
    Context {procedures = proc_map, scopes = insertVarToMap ident t _scopes}

-- | return () is t1 and t2 are the same types, otherwise, throw error
assertType :: Type -> Type -> TC ()
assertType t1 t2 =
  if t1 == t2
    then return ()
    else throwError $ TypeError t1 t2

-- | an empty context with no bindings
emptyContext :: Context
emptyContext = Context {procedures=Map.empty, scopes=[]}

-- | Typechecks a literal
typeCheckLit :: SSMLit -> TC Type
typeCheckLit (LUInt8 _) = return TUInt8
typeCheckLit (LInt32 _) = return TInt32
typeCheckLit (LInt64 _) = return TInt64
typeCheckLit (LUInt64 _) = return TUInt64
typeCheckLit (LBool _) = return TBool
