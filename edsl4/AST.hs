{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables#-}
module AST where

import Control.Monad.Reader

import Data.Proxy
import Data.List.NonEmpty hiding (unzip, zip)

data Type where
    TInt  :: Type
    TBool :: Type
    TFun  :: Type -> Type -> Type

instance Show Type where
    show TInt = "Int"
    show TBool = "Bool"
    show (TFun t1 t2) = show t1 ++ " -> " ++ show t2

class SSMType a where
    typeOf :: proxy a -> Type

instance SSMType Int where
    typeOf _ = TInt

instance SSMType Bool where
    typeOf _ = TBool

instance (SSMType a, SSMType b) => SSMType (a -> b) where
    typeOf _ = TFun (typeOf (undefined :: (Proxy a))) 
                    (typeOf (undefined :: (Proxy b)))

{- | Variables are named values with a phantom type, where the phantom type denotes
the type of the value held by the variable. -}
data Var where
    Variable :: Type -> String -> Var

-- | SSM expressions
data SSMExp where
    Var :: String -> SSMExp                            -- ^ Variables
    Lit :: SSMLit -> SSMExp                            -- ^ Literals
    UOp :: SSMExp -> UnaryOp -> SSMExp             -- ^ Unary operators
    BOp :: SSMExp -> SSMExp -> BinOp -> SSMExp  -- ^ Binary operators

-- | SSM literals
data SSMLit where
    LInt  :: Int  -> SSMLit   -- ^ Integer literals
    LBool :: Bool -> SSMLit  -- ^ Boolean literals

{-- | SSM unary operators. We use phantom types to represent the argument type
and the result type of the operator. E.g At (a :: Ref b) :: UnaryOp Bool. -}
data UnaryOp where
    At :: UnaryOp  -- ^ @-operator

{-- | SSM binary operators. We use phantom types to represent the two argument types and
the result type of the operator. E.g OLT 2 3 :: BinOp Int Int Bool -}
data BinOp where
    OPlus  :: BinOp  -- ^ addition
    OMinus :: BinOp  -- ^ subtraction
    OTimes :: BinOp  -- ^ multiplication
    OLT    :: BinOp  -- ^ less-than
    OEQ    :: BinOp

-- | SSM statements
data SSMStm where
    -- ^ Normal assignment
    Assign :: Var -> SSMExp -> SSMStm
    -- ^ Conditional execution, e.g if 2 < 3 then a = 3 else a = 5
    If     :: SSMExp -> [SSMStm] -> Maybe [SSMStm] -> SSMStm
    -- ^ Looping until an expression becomes false, e.g while True a = a + 1
    While  :: SSMExp -> [SSMStm] -> SSMStm
    -- ^ Delayed assignment, e.g after 2 s a = 3
    After  :: SSMExp -> Var -> SSMExp -> SSMStm
    -- ^ Wait for one of many variables to be written to, e.g wait a, b, c
    Wait   :: NonEmpty Var -> SSMStm
    -- ^ Fork child processes, e.g fork fib(n-1,r1), fib(n-2,r2)
    -- NOTE: expr b demands that all expressions be of the same type. Need to circle back
    -- here and figure out the best way to do this. Keep it like this just to keep moving forward.
    Fork   :: [(Var, [SSMExp])] -> SSMStm

-- | SSM routines
data Routine where
    Routine :: String           -- ^ Name of the routine
            -> [Arg cc]         -- ^ Arguments to the routine
            -> [(Var, SSMExp)]  -- ^ Variable declarations and initializations
            -> [SSMStm]         -- ^ Routine body
            -> Routine

data CC = ByValue | ByReference

-- | Routine arguments
data Arg (valorref :: CC) where
    Arg :: SSMExp -> Arg valorref

-- | SSM programs. A program consists of zero or more routines and one main routine.
type Program = ([Routine], Routine)

{-class Ops tycon1 tycon2 where
    (+.) :: tycon1 a -> tycon2 a -> SSMExp a
    (-.) :: tycon1 a -> tycon2 a -> SSMExp a
    (*.) :: tycon1 a -> tycon2 a -> SSMExp a
    (<.) :: tycon1 a -> tycon2 a -> SSMExp Bool

instance Ops Ref SSMExp where
    r +. e = BOp (Var r) e OPlus
    r -. e = BOp (Var r) e OMinus
    r *. e = BOp (Var r) e OTimes
    r <. e = BOp (Var r) e OLT

instance Ops SSMExp Ref where
    e +. r = BOp e (Var r) OPlus
    e -. r = BOp e (Var r) OMinus
    e *. r = BOp e (Var r) OTimes
    e <. r = BOp e (Var r) OLT

instance Ops SSMExp SSMExp where
    e1 +. e2 = BOp e1 e2 OPlus
    e1 -. e2 = BOp e1 e2 OMinus
    e1 *. e2 = BOp e1 e2 OTimes
    e1 <. e2 = BOp e1 e2 OLT

instance Ops Ref Ref where
    r1 +. r2 = BOp (Var r1) (Var r2) OPlus
    r1 -. r2 = BOp (Var r1) (Var r2) OMinus
    r1 *. r2 = BOp (Var r1) (Var r2) OTimes
    r1 <. r2 = BOp (Var r1) (Var r2) OLT

{- ********** Instances ********** -}

instance Num (SSMExp Int) where
    (+)           = (\e1 e2 -> BOp e1 e2 OPlus)
    (*)           = (\e1 e2 -> BOp e1 e2 OTimes)
    abs           = undefined -- for now
    signum        = undefined -- for now
    fromInteger i = Lit (LInt (fromInteger i))
    negate        = undefined-}