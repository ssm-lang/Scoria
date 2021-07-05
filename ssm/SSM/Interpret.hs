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
