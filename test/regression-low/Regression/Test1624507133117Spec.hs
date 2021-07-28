module Regression.Test1624507133117Spec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = "fun1", args = [Right $ Dynamic ("ref1",Ref TBool),Right $ Dynamic ("ref2",Ref TUInt64)], funs = fromList [("fun1",Procedure {name = "fun1", arguments = [("ref1",Ref TBool),("ref2",Ref TUInt64)], body = [Wait [Dynamic ("ref2",Ref TUInt64)],After (Lit TUInt64 (LUInt64 2743)) (Dynamic ("ref2",Ref TUInt64)) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 49181)) (Lit TUInt64 (LUInt64 27530)) OTimes) (BOp TUInt64 (Lit TUInt64 (LUInt64 3961)) (Lit TUInt64 (LUInt64 1352)) OTimes) OTimes),GetRef (Fresh "v1") TBool (Dynamic ("ref1",Ref TBool)),After (Lit TUInt64 (LUInt64 195)) (Dynamic ("ref2",Ref TUInt64)) (BOp TUInt64 (Lit TUInt64 (LUInt64 32467)) (Lit TUInt64 (LUInt64 53679)) OPlus),GetRef (Fresh "v4") TBool (Dynamic ("ref1",Ref TBool))]})], global_references = []}

spec :: H.Spec
spec = T.correctSpec "Test1624507133117" p
