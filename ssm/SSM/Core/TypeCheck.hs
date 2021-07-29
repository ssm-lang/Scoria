module SSM.Core.TypeCheck where

import SSM.Core.Syntax 
import qualified Data.Map as Map
import System.IO ()

-- | Helper function to check whether the type is an integer
isInt :: Type -> Bool 
isInt TBool = False 
isInt (Ref _) = False  
isInt _ = True 

-- | Unwrap the result of typeCheckExp
unwrapExpRes :: Either TypeError Type -> Type
unwrapExpRes (Left TypeError {expected=_expected, actual=_actual, msg=_msg}) = _actual
unwrapExpRes (Right ty) = ty

-- | Helper function to check whether the expression type matches 
-- the reference type
expMatchRef :: Reference -> SSMExp -> Bool 
expMatchRef (s, ty) expr = 
    case typeCheckExp expr of Left TypeError {expected=_expected, actual=_actual, msg=_msg} -> False 
                              Right actualTy -> actualTy == ty

-- | Typechecks a program
typeCheckProgram :: Program -> Bool 
typeCheckProgram Program {entry=e, args=as, funs=fs} = False 

-- | Checks whether a list of arguments all have the correct type
typeCheckArgs :: [Either SSMExp Reference] -> Bool 
typeCheckArgs [] = True 
typeCheckArgs (Left expr : t) = undefined 
typeCheckArgs (Right ref : t) = undefined 

-- | Typechecks a procedure
typeCheckFunction :: Procedure -> Maybe TypeError
typeCheckFunction Procedure {name=n, arguments=params, body=b} = 
    typeCheckStmLst b

-- | Checks the functions in a String-Procedure map all have the corret type
typeCheckFuns :: Map.Map String Procedure -> Maybe TypeError
typeCheckFuns = 
    Map.foldr f Nothing  
    where
        f func Nothing = typeCheckFunction func
        f func te = te

-- | Typechecks an expression
typeCheckExp :: SSMExp -> Either TypeError Type 
typeCheckExp (Var ty str) = Right ty
typeCheckExp (Lit ty lit) = 
    if actualTy == ty then Right ty
    else Left TypeError {expected=ty, actual=actualTy, msg="The literal's type doesn't match the claimed type"}
    where
        actualTy = typeCheckLit lit
typeCheckExp (UOpE ty expr op) = 
    if actualTy == ty then Right ty
    else Left TypeError {expected=ty, actual=actualTy, msg="The expression's type doesn't match the claimed type"}
    where actualTy = unwrapExpRes (typeCheckExp expr)
typeCheckExp (UOpR ty ref op) = 
    if actualTy == ty then Right ty
    else Left TypeError {expected=ty, actual=actualTy, msg="The expression's type doesn't match the claimed type"}
    where actualTy = typeCheckRef ref
typeCheckExp (BOp ty e1 e2 op) = 
    if actualTy1 == ty && actualTy2 == ty then Right ty
    else Left TypeError {expected=ty, actual=actualTy1, msg="The expressions' type doesn't match the claimed type"}
    where 
        actualTy1 = unwrapExpRes (typeCheckExp e1)
        actualTy2 = unwrapExpRes (typeCheckExp e1)

data TypeError = TypeError {expected::Type, actual::Type, msg::String}

-- | Typechecks a list of statements
typeCheckStmLst :: [Stm] -> Maybe TypeError
typeCheckStmLst [] = Nothing 
typeCheckStmLst (h:t) = 
    case typeCheckStm h of Nothing -> typeCheckStmLst t
                           te -> te

typeCheckForkProcs :: [(String, [Either SSMExp Reference])] -> Maybe TypeError
typeCheckForkProcs [] = Nothing
typeCheckForkProcs ((name, exprOrRef):t) = Nothing  

-- | Typechecks a statement
typeCheckStm :: Stm -> Maybe TypeError
typeCheckStm Skip = Nothing 
typeCheckStm (After exp1 ref exp2) = 
    if isInt (unwrapExpRes (typeCheckExp exp1)) then 
        if expMatchRef ref exp2 then
            Nothing 
        else
            Just TypeError {expected=typeCheckRef ref, 
                            actual=unwrapExpRes (typeCheckExp exp1), msg="Expression type doesn't match the reference"}
    else 
        Just TypeError {expected=undefined, actual=undefined, msg="The time parameter is not an int"}
typeCheckStm (Wait refs) = Nothing 
typeCheckStm (While expr stms) = 
    if unwrapExpRes (typeCheckExp expr) == TBool then 
        typeCheckStmLst stms
    else
        Just TypeError {expected=TBool, actual=unwrapExpRes (typeCheckExp expr), msg="Condition variable is not a bool"}
typeCheckStm (If expr stms1 stms2) = 
    if unwrapExpRes (typeCheckExp expr) == TBool then 
        typeCheckStmLst (stms1 ++ stms2)
    else
        Just TypeError {expected=TBool, actual=unwrapExpRes (typeCheckExp expr), msg="Condition variable is not a bool"}
typeCheckStm (Fork procs) = undefined 
typeCheckStm (SetLocal name ty expr) = 
    if actualTy == ty then 
        Nothing 
    else
        Just TypeError {expected=ty, actual=actualTy, msg="Expression doesn't match the type of the local variable"}
    where
        actualTy = unwrapExpRes (typeCheckExp expr)
typeCheckStm (GetRef name ty ref) = 
    if ty == typeCheckRef ref then
        Nothing 
    else 
        Just TypeError {expected=typeCheckRef ref, actual=ty, msg="Reference type doesn't match the claimed type"}
typeCheckStm (SetRef ref expr) = 
    if expMatchRef ref expr then
            Nothing 
        else
            Just TypeError {expected=typeCheckRef ref, 
                            actual=unwrapExpRes (typeCheckExp expr), msg="Expression type doesn't match the reference"}
typeCheckStm (NewRef name ty expr) = 
    if actualTy == ty then
        Nothing 
    else
        Just TypeError {expected=ty, actual=actualTy, msg="Expression type doesn't match the reference"}
    where
        actualTy = unwrapExpRes (typeCheckExp expr)

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