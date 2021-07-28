module Regression.Test1625068655962Spec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = "fun2", args = [Right $ Dynamic ("ref4",Ref TInt32)], funs = fromList [("fun2",Procedure {name = "fun2", arguments = [("ref4",Ref TInt32)], body = [Wait [Dynamic ("ref4",Ref TInt32)],Wait [Dynamic ("ref4",Ref TInt32)],Fork [("fun2",[Right $ Dynamic ("ref4",Ref TInt32)])]]})], global_references = []}

spec :: H.Spec
spec = T.correctSpec "Test1625068655962" p
