module SSM.Core.TypeCheck where

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

-- | Type to record and report the type error
data TypeError = TypeError {expected::Type, actual::Type, msg::String}

instance Show TypeError where
  show TypeError {expected=_, actual=_, msg=_msg} = _msg

-- | data type for the entries in our environment
data Entry = VarEntry {ty::Type}
           | ProcEntry {params::[(Ident, Type)]}
           | RefEntry {ty::Type}

-- | Helper function to check whether the type is an integer
isInt :: Type -> Bool
isInt TBool = False
isInt (Ref _) = False
isInt _ = True

-- | Helper function to take parameters out of a procedure entry
takeParams :: Entry -> [(Ident, Type)]
takeParams VarEntry {ty=_} = []
takeParams RefEntry {ty=_} = []
takeParams ProcEntry {params=_params} = _params

-- | Unwrap the result of typeCheckExp
unwrapExpRes :: Either TypeError Type -> Type
unwrapExpRes (Left TypeError {expected=_, actual=_actual, msg=_}) = _actual
unwrapExpRes (Right ty) = ty

-- | Checks whether two types are the same
sameTy :: Type -> Type -> Either TypeError ()
sameTy t1 t2 | t1 == t2 = Right ()
             | otherwise = Left TypeError {expected=t1, actual=t2, msg="Type mismatch. Expected: " ++ show t1 ++ " Actual: " ++ show t2}

-- | Takes out the type inside the reference
unwrapRef :: Type -> Type
unwrapRef (Ref t) = t
unwrapRef t = t

-- | Helper function to check whether the expression type matches 
-- the reference type
expMatchRef :: Reference -> SSMExp -> Map.Map Ident Entry -> Either TypeError ()
expMatchRef ref expr env = do
    ty <- typeCheckRef env ref
    expTy <- typeCheckExp expr env
    sameTy (unwrapRef ty) expTy

-- | Checks whether the claimed type of the variable matches how it is stored in 
-- the environment
varMatchEnv :: Type -> Ident -> Map.Map Ident Entry -> Either TypeError ()
varMatchEnv ty name env =
    case Map.lookup name env of 
        Nothing -> Left TypeError {expected=ty, actual=TUInt8, msg="The variable is undefined"}
        Just VarEntry {ty=t} ->
            if t == ty then Right () else
                Left TypeError {expected=t, actual=ty, msg="The stored variable doesn't match its claimed type"}
        Just _ -> Left TypeError {expected=ty, actual=TUInt8, msg="The name is for a function, not variable"}

-- | Typechecks a reference
typeCheckRef :: Map.Map Ident Entry -> Reference -> Either TypeError Type
typeCheckRef env (Dynamic (ident, ty)) = 
    case Map.lookup ident env of 
        Nothing -> Left TypeError {expected=ty, actual=TUInt8, msg="The reference " ++ show ident ++ " is undefined"}
        Just RefEntry {ty=t} ->
            if Ref t == ty then Right ty else
                Left TypeError {expected=t, actual=ty, msg="The stored reference doesn't match its claimed type"}
        Just ProcEntry {params=_} -> Left TypeError {expected=ty, actual=TUInt8, msg="The name " ++ show ident ++ " is for a function, not reference"}
        Just VarEntry {ty=_} -> Left TypeError {expected=ty, actual=TUInt8, msg="The name " ++ show ident ++ " is for a variable, not reference"}
typeCheckRef env (Static (ident, ty)) = 
    case Map.lookup ident env of 
        Nothing -> Left TypeError {expected=ty, actual=TUInt8, msg="The reference is undefined"}
        Just RefEntry {ty=t} ->
            if Ref t == ty then Right ty else
                Left TypeError {expected=t, actual=ty, msg="The stored reference doesn't match its claimed type"}
        Just _ -> Left TypeError {expected=ty, actual=TUInt8, msg="The name is for a function or variable, not reference"}

-- | Checks whether an argument has the correct type
typeCheckArg :: Map.Map Ident Entry -> (Either SSMExp Reference, (Ident, Type)) -> Either TypeError ()
typeCheckArg env (Left expr, (name, ty)) = do
    expTy <- typeCheckExp expr env
    sameTy expTy ty
typeCheckArg env (Right (Dynamic (ident, ty1)), (name, ty2)) = sameTy ty2 ty1
typeCheckArg env (Right (Static (ident, ty1)), (name, ty2)) = sameTy ty2 ty1

-- | Checks whether a list of arguments all have the correct type
typeCheckArgs :: [Either SSMExp Reference] -> [(Ident, Type)] -> Map.Map Ident Entry -> Either TypeError ()
typeCheckArgs args params env = mapM_ (typeCheckArg env) (zip args params)

-- | enter procedure definition into our environment
enterProc :: Map.Map Ident Entry -> Procedure -> Map.Map Ident Entry
enterProc env Procedure {name=_name, arguments=_arguments, body=_} =
    Map.insert _name ProcEntry {params=_arguments} env

-- | enter parameter definitions in the Map `funs` into our environment
enterProcs :: Map.Map Ident Entry -> Map.Map Ident Procedure -> Map.Map Ident Entry
enterProcs env funs = foldl enterProc env (Map.elems funs)

-- | enter the type of the variable into our environment
enterVar :: Map.Map Ident Entry -> (Ident, Type) -> Map.Map Ident Entry
enterVar env (name, ty) = Map.insert name VarEntry{ty=ty} env

-- | enter the type of a reference into our environment
enterRef :: Map.Map Ident Entry -> (Ident, Type) -> Map.Map Ident Entry
enterRef env (name, ty) = Map.insert name RefEntry{ty=ty} env

-- | Typechecks a procedure
typeCheckProcedure ::Map.Map Ident Entry -> Procedure -> Either TypeError ()
typeCheckProcedure env Procedure {name=n, arguments=args, body=b} =
    typeCheckStmLst b newEnv
    where newEnv = foldl enterVar env args

-- | Checks the functions in a String-Procedure map all have the corret type
typeCheckProcs :: Map.Map Ident Procedure -> Map.Map Ident Entry -> Either TypeError ()
typeCheckProcs funs env = mapM_ (typeCheckProcedure env) (Map.elems funs)

-- | Typechecks a program
-- We will first check whether the provided arguments for the entrypoint procedure
-- have the correct types
-- And then, we will check each procedure stored in the [funs] map
typeCheckProgram :: Program -> Either TypeError ()
typeCheckProgram Program {entry=e, funs=fs, globalReferences=gRefs} = 
    typeCheckProcs fs env'
    where
        params = maybe [] takeParams (Map.lookup e env')
        env = enterProcs Map.empty fs
        env' = foldl enterRef env gRefs

-- | Typechecks an expression, meanwhile figuring out the type of the expression
typeCheckExp :: SSMExp -> Map.Map Ident Entry -> Either TypeError Type
typeCheckExp (Var ty ident) env =
    case Map.lookup ident env of Nothing -> Left TypeError {expected=ty, actual=TUInt8, msg="The variable is undefined"}
                                 Just VarEntry {ty=_ty} -> Right _ty
                                 Just _ ->
                                   Left TypeError {expected=ty, actual=TUInt8, msg="The specified name is not for a variable"}
typeCheckExp (Lit ty lit) env =
    if actualTy == ty then Right ty
    else Left TypeError {expected=ty, actual=actualTy, msg="The literal's type doesn't match the claimed type, expected: " ++ show ty ++ " actual: " ++ show actualTy}
    where
        actualTy = typeCheckLit lit
typeCheckExp (UOpE ty expr op) env =
    if actualTy == ty then Right ty
    else Left TypeError {expected=ty, actual=actualTy, msg="The expression's type doesn't match the claimed type"}
    where actualTy = unwrapExpRes (typeCheckExp expr env)
typeCheckExp (UOpR ty ref op) env = do
    actualTy <- typeCheckRef env ref
    if actualTy == ty then Right ty
    else Left TypeError {expected=ty, actual=actualTy, msg="The expression's type doesn't match the claimed type"}
typeCheckExp (BOp ty e1 e2 OEQ) env 
  | actualTy1 == actualTy2 = Right ty
  | otherwise =
    Left TypeError {expected=ty, actual=actualTy1, msg="Cannot compare defferent types"}
  where
      actualTy1 = unwrapExpRes (typeCheckExp e1 env)
      actualTy2 = unwrapExpRes (typeCheckExp e1 env)
typeCheckExp (BOp ty e1 e2 OLT) env 
  | actualTy1 == actualTy2 = Right ty
  | otherwise =
    Left TypeError {expected=ty, actual=actualTy1, msg="Cannot compare defferent types"}
  where
      actualTy1 = unwrapExpRes (typeCheckExp e1 env)
      actualTy2 = unwrapExpRes (typeCheckExp e1 env)
typeCheckExp (BOp ty e1 e2 op) env 
  | actualTy1 == ty && actualTy2 == ty = Right ty
  | actualTy1 /= ty && actualTy2 == ty =
    Left TypeError {expected=ty, actual=actualTy1, msg="The left expressions's type doesn't match the claimed type"}
  | actualTy1 == ty && actualTy2 /= ty =
    Left TypeError {expected=ty, actual=actualTy1, msg="The right expression's type doesn't match the claimed type"}
  | otherwise =
    Left TypeError {expected=ty, actual=actualTy1, msg="Both of the expressions' type doesn't match the claimed type, expected: " ++ show ty ++ " actual: " ++ show actualTy1}
  where
      actualTy1 = unwrapExpRes (typeCheckExp e1 env)
      actualTy2 = unwrapExpRes (typeCheckExp e1 env)

-- | Typechecks a list of statements
typeCheckStmLst :: [Stm] -> Map.Map Ident Entry -> Either TypeError ()
typeCheckStmLst [] env = Right ()
typeCheckStmLst (h:t) env = do
    newEnv <- typeCheckStm h env
    typeCheckStmLst t newEnv

-- | Typechecks one forked procedure
typeCheckForkProc :: Map.Map Ident Entry -> (Ident, [Either SSMExp Reference]) -> Either TypeError ()
typeCheckForkProc env (name, args) =
    typeCheckArgs args params env
    where params = maybe [] takeParams (Map.lookup name env)

-- | Typechecks forked procedures
typeCheckForkProcs :: [(Ident, [Either SSMExp Reference])] -> Map.Map Ident Entry -> Either TypeError ()
typeCheckForkProcs procs env =
    mapM_ (typeCheckForkProc env) procs

-- | Typechecks a statement, meanwhile generating a new environment 
-- with the definitions in the statement
typeCheckStm :: Stm -> Map.Map Ident Entry -> Either TypeError (Map.Map Ident Entry)
typeCheckStm Skip env = Right env
typeCheckStm (After time ref exp2) env = do
        expMatchRef ref exp2 env
        return env
typeCheckStm (Wait refs) env = do
        ty <- mapM_ (typeCheckRef env) refs
        return env
typeCheckStm (While expr stms) env
    | unwrapExpRes (typeCheckExp expr env) == TBool = do
        typeCheckStmLst stms env
        return env
    | otherwise =
        Left TypeError {expected=TBool, actual=unwrapExpRes (typeCheckExp expr env), msg="Condition variable is not a bool"}
typeCheckStm (If expr stms1 stms2) env
    | unwrapExpRes (typeCheckExp expr env) == TBool = do
        typeCheckStmLst (stms1 ++ stms2) env
        return env
    | otherwise =
        Left TypeError {expected=TBool, actual=unwrapExpRes (typeCheckExp expr env), msg="Condition variable is not a bool"}
typeCheckStm (Fork procs) env = do
    typeCheckForkProcs procs env
    return env
typeCheckStm (SetLocal name ty expr) env
    | actualTy == ty = Right (Map.insert name VarEntry {ty=ty} env)
    | otherwise =
        Left TypeError {expected=ty, actual=actualTy, msg="Expression doesn't match the type of the local variable"}
    where
        actualTy = unwrapExpRes (typeCheckExp expr env)
typeCheckStm (SetRef ref expr) env = do
    expMatchRef ref expr env
    return env
typeCheckStm (NewRef name ty expr) env
    | actualTy == ty = Right (Map.insert name RefEntry {ty=ty} env)
    | otherwise =
        Left TypeError {expected=ty, actual=actualTy, msg="Expression type doesn't match the reference"}
    where
        actualTy = unwrapExpRes (typeCheckExp expr env)

-- | Typechecks a literal
typeCheckLit :: SSMLit -> Type
typeCheckLit (LUInt8 _) = TUInt8
typeCheckLit (LInt32 _) = TInt32
typeCheckLit (LInt64 _) = TInt64
typeCheckLit (LUInt64 _) = TUInt64
typeCheckLit (LBool _) = TBool

p :: Program
p = Program
  { entry = Ident "fun0" Nothing
  , funs  = Map.fromList
              [ ( Ident "fun0" Nothing
                , Procedure
                  { name      = Ident "fun0" Nothing
                  , arguments = []
                  , body      =
                    [ NewRef (Ident "ref1" Nothing) TBool  (Lit TBool (LBool True))
                    , NewRef (Ident "ref3" Nothing) TInt32 (Lit TInt32 (LInt32 0))
                    , Fork
                      [(Ident "fun1" Nothing, [Right (Dynamic (Ident "ref11" Nothing, Ref TBool)), Right (Dynamic (Ident "ref3" Nothing, Ref TInt32))])]
                    ]
                  }
                )
              , ( Ident "fun1" Nothing
                , Procedure
                  { name = Ident "fun1" Nothing
                  , arguments = [(Ident "ref12" Nothing, Ref TBool), (Ident "ref3" Nothing, Ref TInt32)]
                  , body = [ After (SSMTime (Lit TUInt64 (LUInt64 2))
                                            SSMNanosecond)
                                   (Dynamic (Ident "ref13" Nothing, Ref TBool))
                                   (Lit TBool (LBool True))
                           , After (SSMTime (Lit TUInt64 (LUInt64 1))
                                            SSMNanosecond)
                                   (Dynamic (Ident "ref3" Nothing, Ref TInt32))
                                   (Lit TInt32 (LInt32 3))
                           , SetRef (Dynamic (Ident "ref3" Nothing, Ref TInt32)) (Lit TInt32 (LInt32 4))
                           , Wait [Dynamic (Ident "ref14" Nothing, Ref TBool)]
                           , Wait [Dynamic (Ident "ref3" Nothing, Ref TInt32)]
                           ]
                  }
                )
              ]
  , globalReferences = []
  }
