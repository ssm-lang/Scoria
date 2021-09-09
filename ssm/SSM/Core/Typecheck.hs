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
                                                , when
                                                )
import qualified Data.Map                      as Map
import qualified SSM.Core.Syntax               as S

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
    procedures :: Map.Map S.Ident [(S.Ident, S.Type)]
  , -- | mapping from variable names to their types
    scope      :: Map.Map S.Ident S.Type
  }

-- | S.Type to record and report the type error.
--
-- Convention: when the two arguments to the constructor of the same type, they
-- designate "ErrorCons expected actual".
data CompilerError
  = -- | variable @Ident@ is not in scope
    UnboundVariable S.Ident
  | -- | S.Types don't match
    TypeError S.Type S.Type
  | -- | Procedure has different name than its key in funs map
    NameError S.Ident S.Ident
  | -- | Number of arguments don't match for procedure call
    ArgsLenError S.Ident Int Int

-- | An empty context with no bindings.
emptyContext :: Context
emptyContext = Context { procedures = Map.empty, scope = Map.empty }

-- | Add new variables to environment scope.
withVars :: [(S.Ident, S.Type)] -> TC a -> TC a
withVars vars =
  local (\ctx -> ctx { scope = Map.union (Map.fromList vars) $ scope ctx })

-- | Add new procedures to the environment.
withProcs :: [S.Procedure] -> TC a -> TC a
withProcs procs = local
  (\ctx -> ctx { procedures = Map.union procsMap $ procedures ctx })
  where procsMap = Map.fromList $ map (\p -> (S.name p, S.arguments p)) procs

-- | Look up the arguments of a procedure.
lookupProcedure :: S.Ident -> TC [(S.Ident, S.Type)]
lookupProcedure ident = asks procedures >>= lookupIdent ident

-- | Look up the type of a variable.
lookupVar :: S.Ident -> TC S.Type
lookupVar ident = asks scope >>= lookupIdent ident

-- | Look up an identifier in a map; throw an error if not found.
lookupIdent :: S.Ident -> Map.Map S.Ident a -> TC a
lookupIdent ident =
  maybe (throwError $ UnboundVariable ident) return . Map.lookup ident

-- | Ensure the two types are equal; throw type error otherwise.
unifyTypes :: S.Type -> S.Type -> TC S.Type
unifyTypes t1 t2 | t1 == t2  = return t1
                 | otherwise = throwError $ TypeError t1 t2

{----- Recursive typechecking -----}

-- Typecheck (and scope-check) a program.
typecheck :: S.Program -> Either CompilerError ()
typecheck prog = runExcept $ flip runReaderT emptyContext $ do
  procs <- forM (Map.toList $ S.funs prog) $ \(n, p) ->
    if n == S.name p then return p else throwError $ NameError n $ S.name p

  -- The global context with which we will check the whole program.
  let withCtx = withVars (S.globalReferences prog) . withProcs procs

  -- Check that the entry point (1) exists and (2) takes no arguments.
  withCtx $ do
    args <- lookupProcedure (S.entry prog)
    unless (null args) $ do
      throwError $ ArgsLenError (S.entry prog) 0 (length args)

  -- Check each procedure defined in the program.
  withCtx $ forM_ procs $ \p ->
    withVars (S.arguments p) $ typecheckStms (S.body p)

-- | Typecheck a list of statements.
typecheckStms :: [S.Stm] -> TC ()
typecheckStms []                             = return ()
typecheckStms (S.After time ref exp2 : stms) = do
  refTy <- typecheckRef ref
  expTy <- typecheckExp exp2
  unifyTypes expTy refTy
  typecheckStms stms
typecheckStms (S.Wait refs : stms) = do
  forM_ refs typecheckRef
  typecheckStms stms
typecheckStms (S.While expr body : stms) = do
  exprTy <- typecheckExp expr
  unifyTypes exprTy S.TBool
  typecheckStms body
  typecheckStms stms
typecheckStms (S.If expr stms1 stms2 : stms) = do
  exprTy <- typecheckExp expr
  unifyTypes exprTy S.TBool
  typecheckStms stms1
  typecheckStms stms2
  typecheckStms stms
typecheckStms (S.SetRef ref expr : stms) = do
  refTy <- typecheckRef ref
  expTy <- typecheckExp expr
  unifyTypes expTy refTy
  typecheckStms stms
typecheckStms (S.SetLocal name ty expr : stms) = do
  actualTy <- typecheckExp expr
  unifyTypes actualTy ty
  withVars [(name, ty)] $ typecheckStms stms
typecheckStms (S.NewRef ident ty expr : stms) = do
  actualTy <- typecheckExp expr
  unifyTypes actualTy ty
  withVars [(ident, ty)] $ typecheckStms stms
typecheckStms (S.Fork procs : stms) = do
  forM_ procs typecheckForkProc
  typecheckStms stms
typecheckStms (S.Skip : stms) = typecheckStms stms

-- | Typechecks an expression, and returns its type.
typecheckExp :: S.SSMExp -> TC S.Type
typecheckExp (S.Var ty ident) = do
  t <- lookupVar ident
  unifyTypes ty t
typecheckExp (S.Lit ty lit) = do
  unifyTypes (typeofLit lit) ty
typecheckExp (S.UOpE ty expr op) = do
  actualTy <- typecheckExp expr
  unifyTypes actualTy ty
typecheckExp (S.UOpR ty ref op) = do
  actualTy <- typecheckRef ref
  unifyTypes actualTy ty
typecheckExp (S.BOp ty e1 e2 op) = do
  actualTy1 <- typecheckExp e1
  actualTy2 <- typecheckExp e2
  unifyTypes actualTy1 actualTy2
  unifyTypes actualTy1 ty

-- | Typecheck a reference, and returns its type.
typecheckRef :: S.Reference -> TC S.Type
typecheckRef (S.Dynamic (ident, ty)) = do
  actualTy <- lookupVar ident
  unifyTypes actualTy ty
typecheckRef (S.Static (ident, ty)) = do
  actualTy <- lookupVar ident
  unifyTypes actualTy ty

-- | Typecheck the invocation of a procedure.
typecheckForkProc :: (S.Ident, [Either S.SSMExp S.Reference]) -> TC ()
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

-- | Obtain type of a literal value.
typeofLit :: S.SSMLit -> S.Type
typeofLit (S.LUInt8  _) = S.TUInt8
typeofLit (S.LInt32  _) = S.TInt32
typeofLit (S.LInt64  _) = S.TInt64
typeofLit (S.LUInt64 _) = S.TUInt64
typeofLit (S.LBool   _) = S.TBool
typeofLit S.LEvent      = S.TEvent
