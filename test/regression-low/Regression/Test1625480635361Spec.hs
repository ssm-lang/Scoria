module Regression.Test1625480635361Spec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = "fun2", args = [Right ("ref2",Ref TUInt64)], funs = fromList [("fun2",Procedure {name = "fun2", arguments = [("ref2",Ref TUInt64)], body = [Fork [("fun5",[Right ("ref2",Ref TUInt64)])],Wait [("ref2",Ref TUInt64)],GetRef (Fresh "v0") TUInt64 ("ref2",Ref TUInt64),GetRef (Fresh "v1") TUInt64 ("ref2",Ref TUInt64)]}),("fun5",Procedure {name = "fun5", arguments = [("ref6",Ref TUInt64)], body = [SetRef ("ref6",Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 48176)) (Lit TUInt64 (LUInt64 17986)) OMinus) (BOp TUInt64 (Lit TUInt64 (LUInt64 57880)) (Lit TUInt64 (LUInt64 33673)) OTimes) OMinus),Wait [("ref6",Ref TUInt64)],After (Lit TUInt64 (LUInt64 1934)) ("ref6",Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 1)) (Lit TUInt64 (LUInt64 1)) OTimes) (BOp TUInt64 (Lit TUInt64 (LUInt64 1)) (Lit TUInt64 (LUInt64 31717)) OTimes) OPlus),Fork [("fun5",[Right ("ref6",Ref TUInt64)])]]})]}

spec :: H.Spec
spec = T.correctSpec "Test1625480635361" p
