{- | The `Compile` monad is used to declare what information should be statically
known in the program. This could be e.g the declaration of global references that
should be visible in the entire program, or it could be IO peripherals.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SSM.Frontend.Compile where

import           SSM.Core                      as SC

import           SSM.Frontend.Syntax
import           SSM.Util.State

import           Control.Monad.State
import           Data.Maybe
import qualified Data.Map as Map

-- | State maintained by the `Compile` monad
data CompileSt backend = CompileSt
    { compileCounter      :: Int                    -- ^ Counter to generate fresh names
    , initialQueueContent :: [QueueContent backend] -- ^ Initial ready-queue content
    , entryPoint          :: Maybe (SSM ())         -- ^ SSM program to run

    , peripherals :: Map.Map String (Peripheral backend) -- ^ Peripherals
    }

-- | Compile monad
newtype Compile backend a = Compile (State (CompileSt backend) a)
  deriving Functor                via State (CompileSt backend)
  deriving Applicative            via State (CompileSt backend)
  deriving Monad                  via State (CompileSt backend)
  deriving (MonadState (CompileSt backend)) via State (CompileSt backend)

{- | @IntState@ instance for `CompileSt` so that the `Compile` monad can generate
fresh names with the generic `SSM.Util.State.fresh` function.. -}
instance IntState (CompileSt backend) where
    getInt = compileCounter
    setInt i st = st { compileCounter = i }

{- | If you have a @Compile (SSM ())@ you have probably set up some global variables
using the @Compile@ monad. This instance makes sure that you can compile and interpret
something that is a program with such global variables. -}
instance SSMProgram backend (Compile backend ()) where
    toProgram (Compile p) =
        let (a, s) = runState
                p
                (CompileSt 0 [] Nothing Map.empty)
            (n, f) = transpile $ fromJust $ entryPoint s
        in  Program (reverse $ SSM.Frontend.Compile.initialQueueContent s) f
                $ Map.elems (SSM.Frontend.Compile.peripherals s)
                
type OutputHandler backend = Handler backend

class Schedulable backend a where
    schedule :: a -> Compile backend ()

instance Schedulable backend (SSM ()) where
    schedule = scheduleSSM

instance Schedulable backend (OutputHandler backend) where
    schedule h = do
        st <- get
        let queuecontents = SSM.Frontend.Compile.initialQueueContent st
            newcontent    = OutputHandler h
            combined      = newcontent : queuecontents
        put $ st { SSM.Frontend.Compile.initialQueueContent = combined }

scheduleSSM :: SSM () -> Compile backend ()
scheduleSSM ssm =
    let id = getProcedureName $ runSSM ssm
    in modify $ \st -> st
        { SSM.Frontend.Compile.initialQueueContent =
            SSMProcedure id [] : SSM.Frontend.Compile.initialQueueContent st
        , entryPoint = Just ssm
        }
