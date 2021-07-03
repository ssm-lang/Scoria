{- | Interpret an SSM program.

The input to the interpreter must be something that has a `SSM.Core.Syntax.SSMProgram`
instance. The output is a list of `SSM.Interpret.Trace.OutputEntry`, which is strictly
used for testing purposes right now. It would make a lot of sense to perhaps break
apart the interpreter and these testing bits so that they are less intertwined.

For more documentation about the interpreter, please refer to the interpreter
implementation.

-}
module SSM.Interpret where

import SSM.Core.Syntax
import qualified SSM.Interpret.Interpreter as I
import SSM.Interpret.Trace

-- | Interpret an SSM program
interpret :: SSMProgram a => a -> Output
interpret = I.interpret . toProgram


-- instead of just having interpret :: SSMProgram a => a -> Output` like we do now,
-- we can do this


{- | Data type of interpreter configuration. Need to modify the interpreter to
interpret a program after loading this information into the interpretation state.
I can hack this together on monday. -}
data InterpretConfig
    = InterpretConfig
    { -- | Size of continuation queue
      contQueueSize  :: Int
      -- | Size of event queue
    , eventQueueSize :: Int
      -- | Program to interpret
    , program        :: Program
    }

-- | Class of types that can be interpreted
class Interpretable a where
    -- | Turn an element of this type into an interpretation configuration
    toConfiguration :: a -> InterpretConfig

-- | Programs can be interpreter with default values
instance Interpretable Program where
    toConfiguration p = InterpretConfig 2048 2048 p

{- | Dummy instance for custom configurations. Don't do anything with them, just
return them as they are given. -}
instance Interpretable InterpretConfig where
    toConfiguration = id

{- | To run a test with custom sizes, use this function to override the default
values. -}
withCustomSizes :: Int -> Int -> Program -> InterpretConfig
withCustomSizes = InterpretConfig

-- | Internal function to interpret from a configuration.
newInterpretFunction :: InterpretConfig -> Output
newInterpretFunction a = undefined -- interpret program

-- | Function you call a program with if you want to interpret it.
interpretAPI :: Interpretable a => a -> Output
interpretAPI = newInterpretFunction . toConfiguration


-- example usage

theprogram :: Program
theprogram = undefined

-- interpret without overriding defaults
f = interpretAPI theprogram

-- interpret with custom queue sizes
g = interpretAPI $ withCustomSizes 8000 8000 theprogram
