{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
module Regression.EmptyFunSpec where

import           Prelude hiding (sum)

import SSM.Language
import SSM.Core
import Data.Map as Map
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T
import Data.Int

fun1 :: SSM ()
fun1 = routine $ return ()

p :: Program
p = Program {initialQueueContent = [SSMProcedure (Ident "fun1" Nothing) []], funs = fromList [(Ident "fun1" Nothing,Procedure {name = Ident "fun1" Nothing, arguments = [], body = []})], peripherals = []}

spec :: H.Spec
spec = T.propSyntacticEquality "EmptyFun" fun1 p
