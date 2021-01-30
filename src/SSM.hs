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
import Control.Monad.Reader

import Data.List.NonEmpty hiding (unzip, zip)

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
    If :: expr Bool -> prog () -> Maybe (prog ()) -> SSMCMD (Param3 prog expr pred) ()
    -- while True a = a + 1
    While :: expr Bool -> prog () -> SSMCMD (Param3 prog expr pred) ()
    -- after 2 s a = 3
    After :: expr Int -> Ref a -> expr b -> SSMCMD (Param3 prog expr pred) ()
    -- wait a, b, c
    Wait :: NonEmpty (Ref a) -> SSMCMD (Param3 prog expr pred) ()
    -- fork fib(n-1,r1), fib(n-2,r2)
    -- NOTE: expr b demands that all expressions be of the same type. Need to circle back
    -- here and figure out the best way to do this.
    -- NODE: Need not be nonempty
    -- NOTE: Inner nonempty needs to be normal ID
    Fork :: NonEmpty (Ref a, NonEmpty (expr b)) -> SSMCMD (Param3 prog expr pred) ()

{- The following three instances are needed for imperative-edls to be able to mix and match
different kind of commands. I will look into how to implement these later. -}
instance HFunctor SSMCMD where
    hfmap f (If c then' (Just else')) = If c (f then') (Just (f else'))
    hfmap f (If c then' Nothing)      = If c (f then') Nothing
    hfmap f (While condition body)    = While condition (f body)
    hfmap _ (After d x v)             = After d x v
    hfmap _ (Wait x)                  = Wait x
    hfmap _ (Fork procedures)         = Fork procedures

instance HBifunctor SSMCMD where
    hbimap f g (If c then' (Just else')) = If (g c) (f then') (Just (f else'))
    hbimap f g (If c then' Nothing)      = If (g c) (f then') Nothing
    hbimap f g (After e1 v e2)           = After (g e1) v (g e2)
    hbimap _ _ (Wait vars)               = Wait vars
    hbimap f g (Fork calls)              = Fork $ nemap (\(v,args) -> (v,nemap g args)) calls
      where nemap = Data.List.NonEmpty.map

instance (SSMCMD :<: instr) => Reexpressible SSMCMD instr env where
    reexpressInstrEnv reexp (If c thn mels) = do
        c' <- reexp c
        ReaderT $ \env ->
            let els' = case mels of
                        Just els -> Just $ runReaderT els env
                        Nothing  -> Nothing
            in singleInj $ If c' (runReaderT thn env) els'
    reexpressInstrEnv reexp (While c body) = do
        c' <- reexp c
        ReaderT $ \env -> singleInj $ While c' (runReaderT body env)
    reexpressInstrEnv reexp (After e1 v e2)  = do
        e1' <- reexp e1
        e2' <- reexp e2
        ReaderT $ \env -> singleInj $ After e1' v e2'
    reexpressInstrEnv reexp (Wait vars) = ReaderT $ \_ -> singleInj $ Wait vars
    reexpressInstrEnv reexp (Fork pc) = do
        let pc'          = toList pc
        let (funs, args) = unzip pc'
        let args'        = Prelude.map toList args
        args''          <- mapM (mapM reexp) args'
        let res          = fromList $ zip funs (Prelude.map fromList args'')
        ReaderT $ \env -> singleInj $ Fork res

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
      => expr Int
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