module Regression.EmptyFunSpec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = "fun1", args = [], funs = fromList [("fun1",Procedure {name = "fun1", arguments = [], body = []})]}

spec :: H.Spec
spec = T.correctSpec "EmptyFun" p
