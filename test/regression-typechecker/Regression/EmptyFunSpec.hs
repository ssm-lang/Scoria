-- | Empty program.
--
-- If this fails, something is severely broken with the compiler pipeline.
module Regression.EmptyFunSpec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = Ident "fun1" Nothing, funs = fromList [(Ident "fun1" Nothing,Procedure {name = Ident "fun1" Nothing, arguments = [], body = []})], globalReferences = []}

spec :: H.Spec
spec = T.correctSpec "EmptyFun" p
