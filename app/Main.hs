module Main where

import Data.List

import Frontend
import Fib
import NonTerminate
import Core
import Trace
import qualified LowCore as LC
import qualified Interpreter as I1
import qualified LowInterpreter as I2
import Data.Time ( diffUTCTime, getCurrentTime )
import System.IO
import Criterion.Main

main :: IO ()
main = defaultMain
  [ bench "new interpreter" $ nf (I2.interpret . LC.transpile) $ myfib 15 inputIntRef
  , bench "old interpreter" $ nf I1.interpret                  $ myfib 15 inputIntRef
  ]