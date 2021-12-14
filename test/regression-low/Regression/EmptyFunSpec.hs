-- | Empty program.
--
-- If this fails, something is severely broken with the compiler pipeline.
module Regression.EmptyFunSpec where

import Data.Map (fromList)
import SSM.Core
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program backend
p = Program {initialQueueContent = [SSMProcedure (Ident "fun1" Nothing) []], funs = fromList [(Ident "fun1" Nothing,Procedure {name = Ident "fun1" Nothing, arguments = [], body = []})], peripherals = []}

spec :: H.Spec
spec = T.correctSpec "EmptyFun" p
