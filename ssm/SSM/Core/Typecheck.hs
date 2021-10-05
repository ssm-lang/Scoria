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
import qualified SSM.Core.Ident                as I
import qualified SSM.Core.Reference            as R
import qualified SSM.Core.Type                 as T
import qualified SSM.Core.Program              as Prog
import qualified SSM.Core.Peripheral           as Peri

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
    procedures :: Map.Map I.Ident [(I.Ident, T.Type)]
  , -- | mapping from variable names to their types
    scope      :: Map.Map I.Ident T.Type
  }

-- | Type to record and report the type error.
--
-- Convention: when the two arguments to the constructor of the same type, they
-- designate "ErrorCons expected actual".
data CompilerError
  = -- | variable @Ident@ is not in scope
    UnboundVariable I.Ident
  | -- | Types don't match
    TypeError T.Type T.Type
  | -- | Procedure has different name than its key in funs map
    NameError I.Ident I.Ident
  | -- | Number of arguments don't match for procedure call
    ArgsLenError I.Ident Int Int
    deriving (Show)

-- | An empty context with no bindings.
emptyContext :: Context
emptyContext = Context { procedures = Map.empty, scope = Map.empty }

-- | Add new variables to environment scope.
withVars :: [(I.Ident, T.Type)] -> TC a -> TC a
withVars vars =
  local (\ctx -> ctx { scope = Map.union (Map.fromList vars) $ scope ctx })

-- | Add new procedures to the environment.
withProcs :: [Prog.Procedure] -> TC a -> TC a
withProcs procs = local
  (\ctx -> ctx { procedures = Map.union procsMap $ procedures ctx })
  where procsMap = Map.fromList $ map (\p -> (name p, arguments p)) procs

-- | Look up the arguments of a procedure.
lookupProcedure :: I.Ident -> TC [(I.Ident, T.Type)]
lookupProcedure ident = asks procedures >>= lookupIdent ident

-- | Look up the type of a variable.
lookupVar :: I.Ident -> TC T.Type
lookupVar ident = asks scope >>= lookupIdent ident

-- | Look up an identifier in a map; throw an error if not found.
lookupIdent :: I.Ident -> Map.Map I.Ident a -> TC a
lookupIdent ident =
  maybe (throwError $ UnboundVariable ident) return . Map.lookup ident

-- | Ensure the two types are equal; throw type error otherwise.
unifyTypes :: T.Type -> T.Type -> TC T.Type
unifyTypes t1 t2 | t1 == t2  = return t1
                 | otherwise = throwError $ TypeError t1 t2

{----- Recursive typechecking -----}

-- Typecheck (and scope-check) a program.
typecheck :: Prog.Program -> Either CompilerError ()
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
typecheckStms :: [S.Stm] -> TC ()
typecheckStms []                             = return ()
typecheckStms (S.After time ref exp2 : stms) = do
  typecheckTime time
  refTy <- typecheckRef ref
  expTy <- typecheckExp exp2
  unifyTypes expTy refTy
  typecheckStms stms
typecheckStms (S.Wait refs : stms) = do
  forM_ refs typecheckRef
  typecheckStms stms
typecheckStms (S.While expr body : stms) = do
  exprTy <- typecheckExp expr
  unifyTypes exprTy TBool
  typecheckStms body
  typecheckStms stms
typecheckStms (S.If expr stms1 stms2 : stms) = do
  exprTy <- typecheckExp expr
  unifyTypes exprTy TBool
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
typecheckExp :: S.SSMExp -> TC T.Type
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
typecheckRef :: R.Reference -> TC T.Type
typecheckRef (R.Dynamic (ident, ty)) = do
  actualTy <- lookupVar ident
  unifyTypes actualTy ty
typecheckRef (R.Static (ident, ty)) = do
  actualTy <- lookupVar ident
  unifyTypes actualTy ty

-- | Typecheck the invocation of a procedure.
typecheckForkProc :: (I.Ident, [Either S.SSMExp R.Reference]) -> TC ()
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

typecheckTime :: S.SSMTime -> TC ()
-- typecheckTime (SSMTimeAdd l r) = typecheckTime l >> typecheckTime r
-- typecheckTime (SSMTimeSub l r) = typecheckTime l >> typecheckTime r
typecheckTime (S.SSMTime a _) = typecheckExp a >>= void . unifyTypes TUInt64

-- | Obtain type of a literal value.
typeofLit :: S.SSMLit -> T.Type
typeofLit (S.LUInt8  _) = TUInt8
typeofLit (S.LInt32  _) = TInt32
typeofLit (S.LInt64  _) = TInt64
typeofLit (S.LUInt64 _) = TUInt64
typeofLit (S.LBool   _) = TBool
typeofLit S.LEvent      = TEvent
