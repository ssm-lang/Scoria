module SSM.Interpret
  ( interpret
  , interpret'
  , interpret_
  , I.InterpretConfig(..)
  , T.Trace(..)
  , T.Event(..)
  ) where

import SSM.Frontend.Compile
import SSM.Core
import SSM.Util.Default
import qualified SSM.Trace.Trace as T

import qualified SSM.Interpret.Interpreter as I

interpret :: I.InterpretConfig -> Compile Interpret () -> T.Trace
interpret cf c = I.interpret cf $ toProgram c

interpret' :: I.InterpretConfig -> Program Interpret -> T.Trace
interpret' cf c = I.interpret cf c

interpret_ :: Compile Interpret () -> T.Trace
interpret_ = I.interpret def . toProgram
