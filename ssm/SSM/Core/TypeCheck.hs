module SSM.Core.TypeCheck where

import SSM.Core.Syntax
    ( Program(..),
      Procedure(..),
      Stm(..),
      SSMLit(..),
      SSMExp(..),
      Reference,
      Type(..),
      getVarName ) 
import qualified Data.Map as Map
import System.IO ()

-- | Type to record and report the type error
data TypeError = TypeError {expected::Type, actual::Type, msg::String}

-- | data type for the entries in our environment
data Entry = VarEntry {ty::Type} 
           | ProcEntry {params::[(String, Type)]}
        --    | RefEntry {ty::Type}

-- | Helper function to check whether the type is an integer
isInt :: Type -> Bool 
isInt TBool = False 
isInt (Ref _) = False  
isInt _ = True 

-- | Helper function to take parameters out of a procedure entry
takeParams :: Entry -> [(String, Type)]
takeParams VarEntry {ty=_} = []
-- takeParams RefEntry {ty=_} = []
takeParams ProcEntry {params=_params} = _params

-- | Unwrap the result of typeCheckExp
unwrapExpRes :: Either TypeError Type -> Type
unwrapExpRes (Left TypeError {expected=_expected, actual=_actual, msg=_msg}) = _actual
unwrapExpRes (Right ty) = ty

-- | Checks whether two types are the same
sameTy :: Type -> Type -> Either TypeError ()
sameTy t1 t2 | t1 == t2 = Right ()
             | otherwise = Left TypeError {expected=t1, actual=t2, msg="The literal's type doesn't match the claimed type"}

-- | Helper function to check whether the expression type matches 
-- the reference type
expMatchRef :: Reference -> SSMExp -> Map.Map String Entry -> Either TypeError ()
expMatchRef (s, ty) expr env = do
    expTy <- typeCheckExp expr env
    sameTy ty expTy

-- | Checks whether the claimed type of the variable matches how it is stored in 
-- the environment
varMatchEnv :: Type -> String -> Map.Map String Entry -> Either TypeError ()
varMatchEnv ty name env = 
    case Map.lookup name env of Nothing -> Left TypeError {expected=ty, actual=TUInt8, msg="The variable is undefined"}
                                Just VarEntry {ty=t} ->
                                    if t == ty then Right () else 
                                        Left TypeError {expected=t, actual=ty, msg="Claimed type unmatched"}
                                Just _ -> Left TypeError {expected=ty, actual=TUInt8, msg="The name is for a function, not variable"}

-- | Checks whether an argument has the correct type
typeCheckArg :: Map.Map String Entry -> (Either SSMExp Reference, (String, Type)) -> Either TypeError ()
typeCheckArg env (Left expr, (name, ty)) = do
    expTy <- typeCheckExp expr env
    sameTy expTy ty
typeCheckArg env (Right (s, ty1), (name, ty2)) = sameTy ty2 ty1 

-- | Checks whether a list of arguments all have the correct type
typeCheckArgs :: [Either SSMExp Reference] -> [(String, Type)] -> Map.Map String Entry -> Either TypeError ()
typeCheckArgs args params env = mapM_ (typeCheckArg env) (zip args params)

-- | enter procedure definition into our environment
enterProc :: Map.Map String Entry -> Procedure -> Map.Map String Entry
enterProc env Procedure {name=_name, arguments=_arguments, body=_} = 
    Map.insert _name ProcEntry {params=_arguments} env

-- | enter parameter definitions in the Map `funs` into our environment
enterProcs :: Map.Map String Entry -> Map.Map String Procedure -> Map.Map String Entry
enterProcs env funs = foldl enterProc env (Map.elems funs) 

-- | enter the type of the variable into our environment
enterVar :: Map.Map String Entry -> (String, Type) -> Map.Map String Entry
enterVar env (name, ty) = Map.insert name VarEntry{ty=ty} env

-- | enter the types of a list of variables into our environment
enterVars :: Map.Map String Entry -> [(String, Type)] -> Map.Map String Entry
enterVars = foldl enterVar

-- | Typechecks a procedure
typeCheckProcedure ::Map.Map String Entry -> Procedure -> Either TypeError ()
typeCheckProcedure env Procedure {name=n, arguments=args, body=b} = 
    typeCheckStmLst b newEnv 
    where newEnv = enterVars env args

-- | Checks the functions in a String-Procedure map all have the corret type
typeCheckProcs :: Map.Map String Procedure -> Map.Map String Entry -> Either TypeError ()
typeCheckProcs funs env = mapM_ (typeCheckProcedure env) (Map.elems funs)

-- | Typechecks a program
-- We will first check whether the provided arguments for the entrypoint procedure
-- have the correct types
-- And then, we will check each procedure stored in the [funs] map
typeCheckProgram :: Program -> Map.Map String Entry -> Either TypeError ()
typeCheckProgram Program {entry=e, args=as, funs=fs} env = do
    res <- typeCheckArgs as params env
    typeCheckProcs fs newEnv
    where 
        params = maybe [] takeParams (Map.lookup e env)
        newEnv = enterProcs env fs

-- | Typechecks an expression, meanwhile figuring out the type of the expression
typeCheckExp :: SSMExp -> Map.Map String Entry -> Either TypeError Type 
typeCheckExp (Var ty str) env = 
    case Map.lookup str env of Nothing -> 
                                Left TypeError {expected=ty, actual=TUInt8, msg="The variable is undefined"}
                               Just VarEntry {ty=_ty} -> Right _ty
                               Just _ -> 
                                Left TypeError {expected=ty, actual=TUInt8, msg="The name is not for a variable"}
typeCheckExp (Lit ty lit) env = 
    if actualTy == ty then Right ty
    else Left TypeError {expected=ty, actual=actualTy, msg="The literal's type doesn't match the claimed type"}
    where
        actualTy = typeCheckLit lit
typeCheckExp (UOpE ty expr op) env = 
    if actualTy == ty then Right ty
    else Left TypeError {expected=ty, actual=actualTy, msg="The expression's type doesn't match the claimed type"}
    where actualTy = unwrapExpRes (typeCheckExp expr env)
typeCheckExp (UOpR ty ref op) env = 
    if actualTy == ty then Right ty
    else Left TypeError {expected=ty, actual=actualTy, msg="The expression's type doesn't match the claimed type"}
    where actualTy = typeCheckRef ref
typeCheckExp (BOp ty e1 e2 op) env = 
    if actualTy1 == ty && actualTy2 == ty then Right ty
    else Left TypeError {expected=ty, actual=actualTy1, msg="The expressions' type doesn't match the claimed type"}
    where 
        actualTy1 = unwrapExpRes (typeCheckExp e1 env)
        actualTy2 = unwrapExpRes (typeCheckExp e1 env)

-- | Typechecks a list of statements
typeCheckStmLst :: [Stm] -> Map.Map String Entry -> Either TypeError ()
typeCheckStmLst [] env = Right ()
typeCheckStmLst (h:t) env = do
    newEnv <- typeCheckStm h env
    typeCheckStmLst t newEnv

-- | Typechecks one forked procedure
typeCheckForkProc :: Map.Map String Entry -> (String, [Either SSMExp Reference]) -> Either TypeError ()
typeCheckForkProc env (name, args) = 
    typeCheckArgs args params env
    where params = maybe [] takeParams (Map.lookup name env)

-- | Typechecks forked procedures
typeCheckForkProcs :: [(String, [Either SSMExp Reference])] -> Map.Map String Entry -> Either TypeError ()
typeCheckForkProcs procs env = 
    mapM_ (typeCheckForkProc env) procs

-- | Typechecks a statement, meanwhile generating a new environment 
-- with the definitions in the statement
typeCheckStm :: Stm -> Map.Map String Entry -> Either TypeError (Map.Map String Entry)
typeCheckStm Skip env = Right env
typeCheckStm (After exp1 ref exp2) env
    | isInt (unwrapExpRes (typeCheckExp exp1 env)) = do 
        expMatchRef ref exp2 env
        return env
    | otherwise =
        Left TypeError {expected=TInt32, actual=unwrapExpRes (typeCheckExp exp1 env), msg="The time parameter is not an int"}
typeCheckStm (Wait refs) env = Right env
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
    | actualTy == ty = Right (Map.insert (getVarName name) VarEntry {ty=ty} env) 
    | otherwise =
        Left TypeError {expected=ty, actual=actualTy, msg="Expression doesn't match the type of the local variable"}
    where
        actualTy = unwrapExpRes (typeCheckExp expr env)
typeCheckStm (GetRef name ty ref) env
    | ty == typeCheckRef ref = Right env
    | otherwise =
        Left TypeError {expected=typeCheckRef ref, actual=ty, msg="Reference type doesn't match the claimed type"}
typeCheckStm (SetRef ref expr) env = do
    expMatchRef ref expr env
    return env
typeCheckStm (NewRef name ty expr) env 
    | actualTy == ty = Right env
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

-- | Typechecks a reference
typeCheckRef :: Reference -> Type 
typeCheckRef (s, ty) = Ref ty 
