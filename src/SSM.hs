-- Need this to be able to use `:+:`
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds#-}
module SSM where

import Language.Embedded.Imperative
import Language.Embedded.Imperative.CMD hiding (If, While)
import qualified Language.Embedded.Imperative.Frontend as FE
import Control.Monad.Operational.Higher

import Data.List.NonEmpty

-- | SSM expressions
data SSMExp a where
    Var :: Ref a    -> SSMExp a                            -- ^ Variables
    Lit :: SSMLit a -> SSMExp a                            -- ^ Literals
    UOp :: SSMExp a -> UnaryOp a b -> SSMExp b             -- ^ Unary operators
    BOp :: SSMExp a -> SSMExp b -> BinOp a b c-> SSMExp c  -- ^ Binary operators

-- | SSM literals
data SSMLit a where
    LInt :: Int -> SSMLit Int     -- ^ Integer literals
    LBool :: Bool -> SSMLit Bool  -- ^ Boolean literals

{-- | SSM unary operators. We use phantom types to represent the argument type
and the result type of the operator. E.g At (a :: Ref b) :: UnaryOp Bool. -}
data UnaryOp a rettype where
    At :: UnaryOp a Bool  -- ^ @-operator

{-- | SSM binary operators. We use phantom types to represent the two argument types and
the result type of the operator. E.g OLT 2 3 :: BinOp Int Int Bool-}
data BinOp a b rettype where
    OPlus  :: BinOp a b rettype  -- ^ addition
    OMinus :: BinOp a b rettype -- ^ subtraction
    OTimes :: BinOp a b rettype -- ^ multiplication
    OLT    :: BinOp a b Bool    -- ^ less-than

-- | SSM commands
data SSMCMD fs a where
    -- if 2 < 3 then a = 3 else a = 5
    If :: expr Bool -> prog () -> Maybe (prog ()) -> SSMCMD (Param3 prog exp pred) ()
    -- while True a = a + 1
    While :: expr Bool -> prog () -> SSMCMD (Param3 prog exp pred) ()
    -- after 2 s a = 3
    After :: expr a -> Ref a -> expr b -> SSMCMD (Param3 prog exp pred) ()
    -- wait a, b, c
    Wait :: NonEmpty (Ref a) -> SSMCMD (Param3 prog exp pred) ()
    -- fork fib(n-1,r1), fib(n-2,r2)
    -- NOTE: expr b demands that all expressions be of the same type. Need to circle back
    -- here and figure out the best way to do this.
    Fork :: NonEmpty (Ref a, NonEmpty (expr b)) -> SSMCMD (Param3 prog exp pred) ()

{- The following three instances are needed for imperative-edls to be able to mix and match
different kind of commands. I will look into how to implement these later. -}
instance HFunctor SSMCMD where
    hfmap _ (After _ _ _) = undefined
    hfmap _ (Wait _)      = undefined
    hfmap _ (Fork _)      = undefined

instance HBifunctor SSMCMD where
    hbimap _ _ (After _ _ _) = undefined
    hbimap _ _ (Wait _)      = undefined
    hbimap _ _ (Fork _)      = undefined

instance (SSMCMD :<: instr) => Reexpressible SSMCMD instr env where
    reexpressInstrEnv reexp (After _ _ _) = undefined
    reexpressInstrEnv reexp (Wait _)      = undefined
    reexpressInstrEnv reexp (Fork _)      = undefined

{- ********** Frontend Functions **********-}
{- These are functions that the users are intended to use. We don't want to expose
internals such as constructors etc. -}

-- | Conditional
if' :: (SSMCMD :<: instr)
   => expr Bool
   -> ProgramT instr (Param2 expr pred) m ()
   -> Maybe (ProgramT instr (Param2 expr pred) m ())
   -> ProgramT instr (Param2 expr pred) m ()
if' condition then' else' = singleInj (If condition then' else')

-- | Loop until a condition becomes false
while :: (SSMCMD :<: instr)
      => expr Bool
      -> ProgramT instr (Param2 expr pred) m ()
      -> ProgramT instr (Param2 expr pred) m ()
while condition body = singleInj (While condition body)

-- | Schedule a future assignment
after :: (pred a, SSMCMD :<: instr)
      => expr a 
      -> Ref a 
      -> expr b 
      -> ProgramT instr (Param2 expr pred) m ()
after e1 r e2 = singleInj (After e1 r e2)

-- | Wait for a variable to be written to
wait :: (pred a, SSMCMD :<: instr)
     => NonEmpty (Ref a)
     -> ProgramT instr (Param2 expr pred) m ()
wait vars = singleInj (Wait vars)

-- | Fork childprocedures and proceed only when they have all terminated
fork :: (pred b, SSMCMD :<: instr)
     => NonEmpty (Ref a, NonEmpty (expr b))
     -> ProgramT instr (Param2 expr pred) m ()
fork procedurecalls = singleInj (Fork procedurecalls)

{- ********** Command things **********-}

type CMD = RefCMD :+: SSMCMD

{- We could perhaps reuse this command? This command contains if and while, which
we want, but it also contains for and break, which we don't want. We could wrap it like this
and thus reuse everything, but then only expose the functionality that we want.? -}
{-newtype ControlFlowCMD fs a = ControlFlowCMD (ControlCMD fs a)
  deriving (HFunctor, HBifunctor) via ControlCMD

instance (ControlCMD :<: instr, ControlFlowCMD :<: instr) => Reexpressible ControlFlowCMD instr env where
    reexpressInstrEnv reexp (ControlFlowCMD cmd) = reexpressInstrEnv reexp cmd

while :: (pred Bool, ControlFlowCMD :<: instr)
      => ProgramT instr (Param2 expr pred) m (expr Bool)
      -> ProgramT instr (Param2 expr pred) m ()
      -> ProgramT instr (Param2 expr pred) m ()
while = FE.while-}