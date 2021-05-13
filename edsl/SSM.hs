module SSM
  ( Ref
  , Exp
  , Lit
  , inputref
  , (+.)
  , (-.)
  , (*.)
  , (<.)
  , (==.)
  , (<~)
  , Frontend.neg
  , int32
  , int64
  , uint64
  , word8
  , true'
  , false'
  , deref
  , var
  , Frontend.wait
  , waitAll
  , after
  , Frontend.fork
  , changed
  , if'
  , while'
  , SSM
  , Program
  , box
  , Output
  , SSM.compile
  , SSM.interpret
  , prettyProgram
) where

import Frontend
import LowCodeGen
import LowCore
import LowInterpreter
import Trace
import LowPretty

compile :: Bool -> Maybe Int -> SSM () -> String
compile b me p = compile_ b me $ transpile p

interpret :: SSM () -> Output
interpret = LowInterpreter.interpret . transpile

prettyPrint :: SSM () -> String
prettyPrint = prettyProgram . transpile