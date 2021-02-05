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

{- | Variables are named values with a phantom type, where the phantom type denotes
the type of the value held by the variable. -}
data Var where
    Variable :: String -> Var
  deriving Show

-- | SSM expressions
data SSMExp where
    Var :: String -> SSMExp                            -- ^ Variables
    Lit :: SSMLit -> SSMExp                            -- ^ Literals
    UOp :: SSMExp -> UnaryOp -> SSMExp             -- ^ Unary operators
    BOp :: SSMExp -> SSMExp -> BinOp -> SSMExp  -- ^ Binary operators
  deriving Show

-- | SSM literals
data SSMLit where
    LInt  :: Int  -> SSMLit   -- ^ Integer literals
    LBool :: Bool -> SSMLit  -- ^ Boolean literals
  deriving Show

{-- | SSM unary operators. We use phantom types to represent the argument type
and the result type of the operator. E.g At (a :: Ref b) :: UnaryOp Bool. -}
data UnaryOp where
    At :: UnaryOp  -- ^ @-operator
  deriving Show

{-- | SSM binary operators. We use phantom types to represent the two argument types and
the result type of the operator. E.g OLT 2 3 :: BinOp Int Int Bool -}
data BinOp where
    OPlus  :: BinOp  -- ^ addition
    OMinus :: BinOp  -- ^ subtraction
    OTimes :: BinOp  -- ^ multiplication
    OLT    :: BinOp  -- ^ less-than
    OEQ    :: BinOp
  deriving Show

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

data Argument = Reference SSMExp | Value SSMExp

-- | SSM routines
data Routine where
    Routine :: String           -- ^ Name of the routine
            -> [Argument]         -- ^ Arguments to the routine
            -> [(Var, SSMExp)]  -- ^ Variable declarations and initializations
            -> [SSMStm]         -- ^ Routine body
            -> Routine

-- | SSM programs. A program consists of zero or more routines and one main routine.
type Program = ([Routine], Routine)