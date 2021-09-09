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

import Control.Monad.Reader -- Reader for context
import Control.Monad.Except -- Except for error handling (synonymous with manually throwing around Either e a, as you are doing now)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..), (<|))

type TC a = ReaderT Context (Except TypeError) a

data Context = Context { procedures :: Map.Map Ident [(Ident, Type)] -- ^ types of procedures
                       , scopes     :: NonEmpty Scope -- ^ "stack" of scopes; head is youngest
                       }

-- | Type to record and report the type error.
--
-- Convention: when the two arguments to the constructor of the same type, they
-- designate "ErrorCons expected actual".
data TypeError = UnboundVariable Ident -- ^ variable @Ident@ is not in scope
               | TypeError Type Type   -- ^ Types don't match
               | NameError Ident Ident -- ^ Procedure has different name than its key in funs map
               | CallError Int Int     -- ^ Number of arguments don't match for procedure call

-- | Mapping from variable names to their types
type Scope = Map.Map Ident Type

emptyContext :: Context
emptyContext = Context {procedures = Map.empty, scopes = NE.fromList [Map.empty]}

-- | Perform a computation with a new scope in the context.
-- Context is restored once the computation terminates.
withNewScope :: TC a -> TC a
withNewScope = local (\c -> c {scopes = Map.empty <| scopes c})

-- | Add new variables to the top of the scope stack
withVars :: [(Ident, Type)] -> TC a -> TC a
withVars vars = local (\ctx -> ctx {scopes = joinScope $ scopes ctx})
  where joinScope (s :| ss) = Map.union (Map.fromList vars) s :| ss

withProcs :: [Procedure] -> TC a -> TC a
withProcs procs = local (\ctx -> ctx {procedures = Map.union procsMap $ procedures ctx})
  where
    procsMap = Map.fromList $ map (\p -> (name p, arguments p)) procs

-- | Lookup an identifier in a map; throw an error if not found.
lookupIdent :: Ident -> Map.Map Ident a -> TC a
lookupIdent ident = maybe (throwError $ UnboundVariable ident) return . Map.lookup ident

-- | Lookup the name and argument types of a procedure.
lookupProcedure :: Ident -> TC [(Ident, Type)]
lookupProcedure ident = asks procedures >>= lookupIdent ident

-- | Look up the type of a variable.
--
-- Map.unions collapses the stack of scopes, with preference for scopes higher
-- in the scope stack (i.e., the younger scopes).
lookupVar :: Ident -> TC Type
lookupVar ident = asks (Map.unions . NE.toList . scopes) >>= lookupIdent ident

-- | Ensures the two types are equal; throw error otherwise.
unifyTypes :: Type -> Type -> TC Type
unifyTypes t1 t2
  | t1 == t2 = return t1
  | otherwise = throwError $ TypeError t1 t2

typeCheck :: Program -> Either TypeError ()
typeCheck p = runExcept $ runReaderT (typeCheckProgram p) emptyContext

-- | Typechecks a program
-- We will first check whether the provided arguments for the entrypoint procedure
-- have the correct types
-- And then, we will check each procedure stored in the [funs] map
typeCheckProgram :: Program -> TC ()
typeCheckProgram prog = do
  procs <- forM (Map.toList $ funs prog) $ \(n, p) ->
    if n == name p
      then return p
      else throwError $ NameError n $ name p
  withVars (globalReferences prog) $ withProcs procs $ forM_ procs typeCheckProc

typeCheckProc :: Procedure -> TC ()
typeCheckProc p = withVars (arguments p) $ typeCheckStms (body p)

-- | Typechecks a statement, meanwhile generating a new environment 
-- with the definitions in the statement
typeCheckStms :: [Stm] -> TC ()
typeCheckStms [] = return ()
typeCheckStms (Skip : stms) = typeCheckStms stms
typeCheckStms (After time ref exp2 : stms) = do
  refTy <- typeCheckRef ref
  expTy <- typeCheckExp exp2
  unifyTypes expTy refTy
  typeCheckStms stms
typeCheckStms (Wait refs : stms) = do
  forM_ refs typeCheckRef
  typeCheckStms stms
typeCheckStms (While expr body : stms) = do
  exprTy <- typeCheckExp expr
  unifyTypes exprTy TBool
  withNewScope $ typeCheckStms body
  typeCheckStms stms
typeCheckStms (If expr stms1 stms2 : stms) = do
  exprTy <- typeCheckExp expr
  unifyTypes exprTy TBool
  withNewScope $ typeCheckStms stms1
  withNewScope $ typeCheckStms stms2
  typeCheckStms stms
typeCheckStms (SetRef ref expr : stms) = do
  refTy <- typeCheckRef ref
  expTy <- typeCheckExp expr
  unifyTypes expTy refTy
  typeCheckStms stms
typeCheckStms (SetLocal name ty expr : stms) = do
  actualTy <- typeCheckExp expr
  unifyTypes actualTy ty
  withVars [(name, ty)] $ typeCheckStms stms
typeCheckStms (NewRef ident ty expr : stms) = do
  actualTy <- typeCheckExp expr
  unifyTypes actualTy ty
  withVars [(ident, ty)] $ typeCheckStms stms
typeCheckStms (Fork procs : stms) = do
  forM_ procs typeCheckForkProc
  typeCheckStms stms

-- | Typechecks an expression, meanwhile figuring out the type of the expression
typeCheckExp :: SSMExp -> TC Type
typeCheckExp (Var ty ident) = do
    t <- lookupVar ident
    unifyTypes ty t
typeCheckExp (Lit ty lit) = do
    unifyTypes (typeofLit lit) ty
typeCheckExp (UOpE ty expr op) = do
    actualTy <- typeCheckExp expr
    unifyTypes actualTy ty
typeCheckExp (UOpR ty ref op) = do
    actualTy <- typeCheckRef ref
    unifyTypes actualTy ty
typeCheckExp (BOp ty e1 e2 op) = do
    actualTy1 <- typeCheckExp e1
    actualTy2 <- typeCheckExp e2
    unifyTypes actualTy1 actualTy2
    unifyTypes actualTy1 ty

-- | Typechecks a reference
typeCheckRef :: Reference -> TC Type
typeCheckRef (Dynamic (ident, ty)) = do
    actualTy <- lookupVar ident
    unifyTypes actualTy ty
typeCheckRef (Static (ident, ty)) = do
    actualTy <- lookupVar ident
    unifyTypes actualTy ty

-- | Typechecks one forked procedure.
typeCheckForkProc :: (Ident, [Either SSMExp Reference]) -> TC ()
typeCheckForkProc (name, actuals) = do
    formals <- lookupProcedure name
    when (length formals /= length actuals) $ throwError $ CallError (length formals) (length actuals)
    forM_ (zip formals actuals) $ \((formal, fty), actual) -> do
      aty <- typeCheckActual actual
      unifyTypes fty aty
  where typeCheckActual (Left actualExp) = typeCheckExp actualExp
        typeCheckActual (Right actualRef) = typeCheckRef actualRef

-- | Obtains type of a literal value.
typeofLit :: SSMLit -> Type
typeofLit (LUInt8 _) = TUInt8
typeofLit (LInt32 _) = TInt32
typeofLit (LInt64 _) = TInt64
typeofLit (LUInt64 _) = TUInt64
typeofLit (LBool _) = TBool
typeofLit LEvent = TEvent
