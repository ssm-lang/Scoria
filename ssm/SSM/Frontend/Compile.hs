{- | The `Compile` monad is used to declare what information should be statically
known in the program. This could be e.g the declaration of global references that
should be visible in the entire program, or it could be IO peripherals.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
module SSM.Frontend.Compile where

import           SSM.Core.Peripheral.GPIO
import           SSM.Core.Peripheral.LED
import           SSM.Core.Syntax hiding (initialQueueContent)
import           SSM.Frontend.Syntax
import           SSM.Util.State

import           Control.Monad.State
import           Data.Maybe

-- | State maintained by the `Compile` monad
data CompileSt = CompileSt
    { compileCounter      :: Int              -- ^ Counter to generate fresh named
    , initialQueueContent :: [QueueContent]  -- ^ Initial ready-queue content
    , entryPoint          :: Maybe (SSM ())  -- ^ SSM program to run

    -- globals & peripherals
    , generatedGlobals    :: [(Ident, Type)]  -- ^ Names and types of global references
    , gpioperipherals     :: GPIOPeripheral   -- ^ GPIO peripherals
    , ledperipherals      :: LEDPeripheral    -- ^ LED peripheral
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

-- | Add the name and type of a global variable to the Compile monad
addGlobal :: Ident -> Type -> Compile ()
addGlobal name t = do
    s <- get
    if name `elem` map fst (generatedGlobals s)
        then error $ concat
            [ "name "
            , identName name
            , " has already been declared as a global variable"
            ]
        else
            modify $ \st ->
                st { generatedGlobals = generatedGlobals st ++ [(name, t)] }

{- | Meant to be used in infix position, like @st1 `hasSameGlobalsAs` st2@. Returns
@True@ if, as the name suggests, the two compile states has the same global references
declared. -}
hasSameGlobalsAs :: CompileSt -> CompileSt -> Bool
hasSameGlobalsAs st1 st2 = generatedGlobals st1 == generatedGlobals st2

{- | Rename the newst declared global reference by giving it the name that is supplied
to this function.

NOTE: This function is only meant to be called by the BinderAnn library. -}
renameNewestGlobal :: Ident -> Compile ()
renameNewestGlobal name = do
    st <- get
    let newest = last (generatedGlobals st)
    modify $ \st -> st
        { generatedGlobals = init (generatedGlobals st) ++ [(name, snd newest)]
        }

{- | If you have a @Compile (SSM ())@ you have probably set up some global variables
using the @Compile@ monad. This instance makes sure that you can compile and interpret
something that is a program with such global variables. -}
instance SSMProgram (Compile ()) where
    toProgram (Compile p) =
        let (a, s) = runState p (CompileSt 0 [] Nothing[] emptyGPIOPeripheral emptyLEDPeripheral)
            (n, f) = transpile $ fromJust $ entryPoint s
        in  Program (reverse $ initialQueueContent s)
                    f
                    (generatedGlobals s)
                    (Just $ SSM.Frontend.Compile.gpioperipherals s)
                    (Just $ SSM.Frontend.Compile.ledperipherals s)

-- | Class of types that can be scheduled for execution by the SSM RTS
class Schedulable a where
    -- | Schedule an @a@ for execution by the SSM runtime system
    schedule :: a -> Compile ()

-- | Handlers can be scheduled for execution by the SSM RTS
instance Schedulable Handler where
    schedule h@(StaticOutputHandler ref id) =
        modify $ \st ->
            st { initialQueueContent = Handler h : initialQueueContent st }

-- | `SSM` programs can be scheduled for execution by the SSM RTS
instance Schedulable (SSM ()) where
    schedule ssm =
        let id = getProcedureName $ runSSM ssm
        in  modify $ \st -> st
                { initialQueueContent = SSMProcedure id []
                                            : initialQueueContent st
                , entryPoint          = Just ssm
                }
