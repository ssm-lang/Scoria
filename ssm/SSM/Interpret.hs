module SSM.Interpret where

import SSM.Frontend.Syntax
import qualified SSM.Interpret.Interpreter as I
import SSM.Interpret.Trace

-- | Interpret an SSM program
interpret :: SSM () -> Output
interpret = I.interpret . transpile