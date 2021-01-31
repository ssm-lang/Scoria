{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module AST where

import Control.Monad.Reader

import Data.List.NonEmpty hiding (unzip, zip)

{- | Variables are named values with a phantom type, where the phantom type denotes
the type of the value held by the variable. -}
data Ref a where
    Ref :: String -> Ref a

-- | SSM expressions
data SSMExp a where
    Var :: Ref a    -> SSMExp a                      -- ^ Variables
    Lit :: SSMLit a -> SSMExp a                            -- ^ Literals
    UOp :: SSMExp a -> UnaryOp a b -> SSMExp b             -- ^ Unary operators
    BOp :: SSMExp a -> SSMExp b -> BinOp a b c-> SSMExp c  -- ^ Binary operators

-- | SSM literals
data SSMLit a where
    LInt  :: Int  -> SSMLit Int   -- ^ Integer literals
    LBool :: Bool -> SSMLit Bool  -- ^ Boolean literals

{-- | SSM unary operators. We use phantom types to represent the argument type
and the result type of the operator. E.g At (a :: Ref b) :: UnaryOp Bool. -}
data UnaryOp a rettype where
    At :: UnaryOp a Bool  -- ^ @-operator

{-- | SSM binary operators. We use phantom types to represent the two argument types and
the result type of the operator. E.g OLT 2 3 :: BinOp Int Int Bool -}
data BinOp a b rettype where
    OPlus  :: BinOp a b rettype  -- ^ addition
    OMinus :: BinOp a b rettype  -- ^ subtraction
    OTimes :: BinOp a b rettype  -- ^ multiplication
    OLT    :: BinOp a b Bool     -- ^ less-than

-- | SSM statements
data SSMStm where
    -- ^ Normal assignment
    Assign :: Ref a -> SSMExp a -> SSMStm
    -- ^ Conditional execution, e.g if 2 < 3 then a = 3 else a = 5
    If     :: SSMExp Bool -> [SSMStm] -> Maybe [SSMStm] -> SSMStm
    -- ^ Looping until an expression becomes false, e.g while True a = a + 1
    While  :: SSMExp Bool -> [SSMStm] -> SSMStm
    -- ^ Delayed assignment, e.g after 2 s a = 3
    After  :: SSMExp Int -> Ref b -> SSMExp b -> SSMStm
    -- ^ Wait for one of many variables to be written to, e.g wait a, b, c
    Wait   :: NonEmpty (Ref a) -> SSMStm
    -- ^ Fork child processes, e.g fork fib(n-1,r1), fib(n-2,r2)
    -- NOTE: expr b demands that all expressions be of the same type. Need to circle back
    -- here and figure out the best way to do this. Keep it like this just to keep moving forward.
    Fork   :: [(Ref a, [SSMExp b])] -> SSMStm

-- | SSM routines
data Routine where
    Routine :: String               -- ^ Name of the routine
            -> [Arg a]              -- ^ Arguments to the routine
            -> [(Ref a, SSMExp a)]  -- ^ Variable declarations and initializations
            -> [SSMStm]             -- ^ Routine body
            -> Routine

-- | Routine arguments
data Arg a where
    ByLit :: SSMExp a -> Arg a
    ByVal :: Ref a -> Arg a  -- ^ Normal, by value, parameters
    ByRef :: Ref a -> Arg a  -- ^ Reference parameters. Use them like normal variables, with the
                             -- ^ understanding that they will change a value in at least one other
                             -- ^ routine.

-- | SSM programs. A program consists of zero or more routines and one main routine.
type Program = ([Routine], Routine)

{- ********** Instances ********** -}

instance Num (SSMExp Int) where
    (+)           = (\e1 e2 -> BOp e1 e2 OPlus)
    (*)           = (\e1 e2 -> BOp e1 e2 OTimes)
    abs           = undefined -- for now
    signum        = undefined -- for now
    fromInteger i = Lit (LInt (fromInteger i))
    negate        = undefined