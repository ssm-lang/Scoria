module Regression.Test1624507133117Spec where

import Data.Map (fromList)
import LowCore
import qualified Test.Ssm.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = "fun1", args = [Right ("ref1",Ref TBool),Right ("ref2",Ref TUInt64)], funs = fromList [("fun1",Procedure {name = "fun1", arguments = [("ref1",Ref TBool),("ref2",Ref TUInt64)], body = [Wait [("ref2",Ref TUInt64)],After (Lit TUInt64 (LUInt64 2743)) ("ref2",Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 49181)) (Lit TUInt64 (LUInt64 27530)) OTimes) (BOp TUInt64 (Lit TUInt64 (LUInt64 3961)) (Lit TUInt64 (LUInt64 1352)) OTimes) OTimes),GetRef (Fresh "v1") TBool ("ref1",Ref TBool),After (Lit TUInt64 (LUInt64 195)) ("ref2",Ref TUInt64) (BOp TUInt64 (Lit TUInt64 (LUInt64 32467)) (Lit TUInt64 (LUInt64 53679)) OPlus),GetRef (Fresh "v4") TBool ("ref1",Ref TBool)]})]}

spec :: H.Spec
spec = T.doProgramSpec "Test1624507133117" p
