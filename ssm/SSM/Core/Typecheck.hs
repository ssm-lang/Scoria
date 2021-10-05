module SSM.Core.Typecheck
  ( typecheck
  ) where

import           Control.Monad.Except           ( Except
                                                , MonadError(..)
                                                , runExcept
                                                )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                , asks
                                                , forM
                                                , forM_
                                                , unless
                                                , void
                                                , when
                                                )
import qualified Data.Map                      as Map
import qualified SSM.Core.Syntax               as S
import SSM.Core.Ident
import SSM.Core.Type
import SSM.Core.Program
import SSM.Core.Peripheral

{----- Typechecker context -----}

-- | The typechecking monad.
--
-- Uses the @Except@ monad to throw errors, and the @ReaderT@ monad to maintain
-- the type environment. We use @ReaderT@ instead of @StateT@ to maintain that
-- environment so that updates to the environment are local by default, and are
-- not persisted unless explicitly indicated. This helps avoid forgetting to
-- "pop" the scope stack.
type TC a = ReaderT Context (Except CompilerError) a

-- | The state carried by the typechecker.
data Context = Context
  { -- | types of procedures
    procedures :: Map.Map Ident [(Ident, Type)]
  , -- | mapping from variable names to their types
    scope      :: Map.Map Ident Type
  }

-- | Type to record and report the type error.
--
-- Convention: when the two arguments to the constructor of the same type, they
-- designate "ErrorCons expected actual".
data CompilerError
  = -- | variable @Ident@ is not in scope
    UnboundVariable Ident
  | -- | Types don't match
    TypeError Type Type
  | -- | Procedure has different name than its key in funs map
    NameError Ident Ident
  | -- | Number of arguments don't match for procedure call
    ArgsLenError Ident Int Int
    deriving (Show)

-- | An empty context with no bindings.
emptyContext :: Context
emptyContext = Context { procedures = Map.empty, scope = Map.empty }

-- | Add new variables to environment scope.
withVars :: [(Ident, Type)] -> TC a -> TC a
withVars vars =
  local (\ctx -> ctx { scope = Map.union (Map.fromList vars) $ scope ctx })

-- | Add new procedures to the environment.
withProcs :: [Procedure] -> TC a -> TC a
withProcs procs = local
  (\ctx -> ctx { procedures = Map.union procsMap $ procedures ctx })
  where procsMap = Map.fromList $ map (\p -> (name p, arguments p)) procs

-- | Look up the arguments of a procedure.
lookupProcedure :: Ident -> TC [(Ident, Type)]
lookupProcedure ident = asks procedures >>= lookupIdent ident

-- | Look up the type of a variable.
lookupVar :: Ident -> TC Type
lookupVar ident = asks scope >>= lookupIdent ident

-- | Look up an identifier in a map; throw an error if not found.
lookupIdent :: Ident -> Map.Map Ident a -> TC a
lookupIdent ident =
  maybe (throwError $ UnboundVariable ident) return . Map.lookup ident

-- | Ensure the two types are equal; throw type error otherwise.
unifyTypes :: Type -> Type -> TC Type
unifyTypes t1 t2 | t1 == t2  = return t1
                 | otherwise = throwError $ TypeError t1 t2

{----- Recursive typechecking -----}

-- Typecheck (and scope-check) a program.
typecheck :: Program -> Either CompilerError ()
typecheck prog = runExcept $ flip runReaderT emptyContext $ do
  procs <- forM (Map.toList $ funs prog) $ \(n, p) ->
    if n == name p then return p else throwError $ NameError n $ name p

  -- The global context with which we will check the whole program.
  let withCtx = withVars (globalReferences prog) . withProcs procs

  -- Check that the entry point (1) exists and (2) takes no arguments.
  withCtx $ do
    args <- lookupProcedure (entry prog)
    unless (null args) $ do
      throwError $ ArgsLenError (entry prog) 0 (length args)

  -- Check each procedure defined in the program.
  withCtx $ forM_ procs $ \p ->
    withVars (arguments p) $ typecheckStms (body p)

-- | Typecheck a list of statements.
typecheckStms :: [Stm] -> TC ()
typecheckStms []                             = return ()
typecheckStms (After time ref exp2 : stms) = do
  typecheckTime time
  refTy <- typecheckRef ref
  expTy <- typecheckExp exp2
  unifyTypes expTy refTy
  typecheckStms stms
typecheckStms (Wait refs : stms) = do
  forM_ refs typecheckRef
  typecheckStms stms
typecheckStms (While expr body : stms) = do
  exprTy <- typecheckExp expr
  unifyTypes exprTy TBool
  typecheckStms body
  typecheckStms stms
typecheckStms (If expr stms1 stms2 : stms) = do
  exprTy <- typecheckExp expr
  unifyTypes exprTy TBool
  typecheckStms stms1
  typecheckStms stms2
  typecheckStms stms
typecheckStms (SetRef ref expr : stms) = do
  refTy <- typecheckRef ref
  expTy <- typecheckExp expr
  unifyTypes expTy refTy
  typecheckStms stms
typecheckStms (SetLocal name ty expr : stms) = do
  actualTy <- typecheckExp expr
  unifyTypes actualTy ty
  withVars [(name, ty)] $ typecheckStms stms
typecheckStms (NewRef ident ty expr : stms) = do
  actualTy <- typecheckExp expr
  unifyTypes actualTy ty
  withVars [(ident, ty)] $ typecheckStms stms
typecheckStms (Fork procs : stms) = do
  forM_ procs typecheckForkProc
  typecheckStms stms
typecheckStms (Skip : stms) = typecheckStms stms

-- | Typechecks an expression, and returns its type.
typecheckExp :: SSMExp -> TC Type
typecheckExp (Var ty ident) = do
  t <- lookupVar ident
  unifyTypes ty t
typecheckExp (Lit ty lit) = do
  unifyTypes (typeofLit lit) ty
typecheckExp (UOpE ty expr op) = do
  actualTy <- typecheckExp expr
  unifyTypes actualTy ty
typecheckExp (UOpR ty ref op) = do
  actualTy <- typecheckRef ref
  unifyTypes actualTy ty
typecheckExp (BOp ty e1 e2 op) = do
  actualTy1 <- typecheckExp e1
  actualTy2 <- typecheckExp e2
  unifyTypes actualTy1 actualTy2
  unifyTypes actualTy1 ty

-- | Typecheck a reference, and returns its type.
typecheckRef :: Reference -> TC Type
typecheckRef (Dynamic (ident, ty)) = do
  actualTy <- lookupVar ident
  unifyTypes actualTy ty
typecheckRef (Static (ident, ty)) = do
  actualTy <- lookupVar ident
  unifyTypes actualTy ty

-- | Typecheck the invocation of a procedure.
typecheckForkProc :: (Ident, [Either SSMExp Reference]) -> TC ()
typecheckForkProc (name, actuals) = do
  formals <- lookupProcedure name

  when (length formals /= length actuals) $ do
    throwError $ ArgsLenError name (length formals) (length actuals)

  forM_ (zip formals actuals) $ \((formal, fty), actual) -> do
    aty <- typecheckActual actual
    unifyTypes fty aty
 where
  typecheckActual (Left  actualExp) = typecheckExp actualExp
  typecheckActual (Right actualRef) = typecheckRef actualRef

typecheckTime :: SSMTime -> TC ()
typecheckTime (SSMTimeAdd l r) = typecheckTime l >> typecheckTime r
typecheckTime (SSMTimeSub l r) = typecheckTime l >> typecheckTime r
typecheckTime (SSMTime a _) = typecheckExp a >>= void . unifyTypes TUInt64

-- | Obtain type of a literal value.
typeofLit :: SSMLit -> Type
typeofLit (LUInt8  _) = TUInt8
typeofLit (LInt32  _) = TInt32
typeofLit (LInt64  _) = TInt64
typeofLit (LUInt64 _) = TUInt64
typeofLit (LBool   _) = TBool
typeofLit LEvent      = TEvent
