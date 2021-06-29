module SSM.Interpret where

import SSM.Core.Syntax
import qualified SSM.Interpret.Interpreter as I
import SSM.Interpret.Trace

-- | Interpret an SSM program
interpret :: SSMProgram a => a -> Output
interpret = I.interpret . toProgram