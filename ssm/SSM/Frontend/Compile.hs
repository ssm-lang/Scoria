{- | The `Compile` monad is used to declare what information should be statically
known in the program. This could be e.g the declaration of global references that
should be visible in the entire program, or it could be IO peripherals.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module SSM.Frontend.Compile where

import           SSM.Core                      as SC

import           SSM.Frontend.Syntax
import           SSM.Util.State

import           Control.Monad.State
import           Data.Maybe

-- | State maintained by the `Compile` monad
data CompileSt = CompileSt
    { compileCounter      :: Int                 -- ^ Counter to generate fresh named
    , initialQueueContent :: [QueueContent]      -- ^ Initial ready-queue content
    , entryPoint          :: Maybe (SSM ())      -- ^ SSM program to run

    -- globals & peripherals
    , generatedGlobals    :: IdentityPeripheral  -- ^ Names and types of global
    , gpioperipherals     :: GPIOPeripheral      -- ^ GPIO peripherals
    , ledperipherals      :: LEDPeripheral       -- ^ LED peripheral
    , basicblePeripheral  :: Maybe BasicBLE      -- ^ Basic BLE peripheral
    }

-- | Compile monad
newtype Compile a = Compile (State CompileSt a)
  deriving Functor                via State CompileSt
  deriving Applicative            via State CompileSt
  deriving Monad                  via State CompileSt
  deriving (MonadState CompileSt) via State CompileSt

{- | @IntState@ instance for `CompileSt` so that the `Compile` monad can generate
fresh names with the generic `SSM.Util.State.fresh` function.. -}
instance IntState CompileSt where
    getInt = compileCounter
    setInt i st = st { compileCounter = i }

{- | If you have a @Compile (SSM ())@ you have probably set up some global variables
using the @Compile@ monad. This instance makes sure that you can compile and interpret
something that is a program with such global variables. -}
instance ( IsPeripheral backend GPIOPeripheral
         , IsPeripheral backend LEDPeripheral
         , IsPeripheral backend IdentityPeripheral
         , IsPeripheral backend BasicBLE) => SSMProgram backend (Compile ()) where
    toProgram (Compile p) =
        let (a, s) = runState
                p
                (CompileSt 0
                           []
                           Nothing
                           emptyIdentityPeripheral
                           emptyGPIOPeripheral
                           emptyLEDPeripheral
                           Nothing
                )
            (n, f) = transpile $ fromJust $ entryPoint s
        in  Program (reverse $ SSM.Frontend.Compile.initialQueueContent s) f
                $  [ Peripheral $ SSM.Frontend.Compile.gpioperipherals s
                   , Peripheral $ SSM.Frontend.Compile.ledperipherals s
                   , Peripheral $ generatedGlobals s
                   ]
                ++ maybe [] (\p -> [Peripheral p]) (basicblePeripheral s)

{- | Schedule an SSM procedure for execution upon program start-up. Procedures that are
scheduled will be placed in the ready queue to be executed when the program starts.
Currently, they are executed in the order in which they were scheduled.

Note that there are only two valid things that can be scheduled.

  1. Nullary SSM procedures, aka procedures that take no arguments. These procedures must
  be created by using the @Box@ machinery.
  2. Handlers that are returned by creating peripherals.

It is forbidden to schedule stuff like @fork [ ... ]@, @ var 0 >>= \r -> assign r 5@ and
so forth.

-}
schedule :: SSM () -> Compile ()
schedule ssm = do
    case isHandler ssm of
        Just handlers -> modify $ \st -> st
            { SSM.Frontend.Compile.initialQueueContent = map SC.Handler handlers
                ++ SSM.Frontend.Compile.initialQueueContent st
            }
        Nothing ->
            let id = getProcedureName $ runSSM ssm
            in
                modify $ \st -> st
                    { SSM.Frontend.Compile.initialQueueContent =
                        SSMProcedure id []
                            : SSM.Frontend.Compile.initialQueueContent st
                    , entryPoint                               = Just ssm
                    }
