module SSM.Core.TypeCheck where

import SSM.Core.Syntax 
import qualified Data.Map as Map
import System.IO ()

-- | Helper function to check whether the type is an integer
isInt :: Type -> Bool 
isInt TBool = False 
isInt (Ref _) = False  
isInt _ = True 

-- | Helper function to check whether the expression type matches 
-- the reference type
expMatchRef :: Reference -> SSMExp -> Bool 
expMatchRef (s, ty) expr = 
    typeCheckExp expr == ty

-- | Typechecks a program
typeCheckProgram :: Program -> Bool 
typeCheckProgram Program {entry=e, args=as, funs=fs} = False 

-- | Checks whether a list of arguments all have the correct type
typeCheckArgs :: [Either SSMExp Reference] -> Bool 
typeCheckArgs [] = True 
typeCheckArgs (Left expr : t) = typeCheckExp expr && typeCheckArgs t
typeCheckArgs (Right ref: t) = typeCheckRef ref && typeCheckArgs t

-- | Typechecks a procedure
typeCheckFunction :: Procedure -> Bool 
typeCheckFunction Procedure {name=n, arguments=params, body=b} = False 

-- | Checks the functions in a String-Procedure map all have the corret type
typeCheckFuns :: Map.Map String Procedure -> Bool 
typeCheckFuns = 
    Map.foldr f True 
    where
        f func cur = cur && typeCheckFunction func

-- | Typechecks an expression
typeCheckExp :: SSMExp -> Type 
typeCheckExp (Var ty str) = ty
typeCheckExp (Lit ty lit) = ty
typeCheckExp (UOpE ty expr op) = ty
typeCheckExp (UOpR ty ref op) = ty
typeCheckExp (BOp ty e1 e2 op) = ty

-- | Typechecks a statement
typeCheckStm :: Stm -> IO Type
typeCheckStm Skip = return TUInt8 
typeCheckStm (After exp1 ref exp2) = 
    if isInt (typeCheckExp exp1) then 
        if expMatchRef ref exp2 then
            return TUInt8
        else
            do putStrLn "expression type doesn't match reference"
               return TUInt8 
    else 
        do putStrLn "Time constant has to be an int"
           return TUInt8 
typeCheckStm (Wait refs) = return TUInt8 
-- typeCheckStm (While expr stms) = True 
-- typeCheckStm (If expr stms1 stms2) = True 
-- typeCheckStm (Fork procs) = True 
-- typeCheckStm (SetLocal name ty expr) = True 
-- typeCheckStm (GetRef name ty ref) = True 
-- typeCheckStm (SetRef ref expr) = True 
-- typeCheckStm (NewRef name ty expr) = True 

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