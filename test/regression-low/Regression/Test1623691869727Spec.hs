module Regression.Test1623691869727Spec where

import Data.Map (fromList)
import LowCore
import qualified Test.Ssm.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = "fun3", args = [Right $ Dynamic ("ref1",Ref TUInt64)], funs = fromList [("fun3",Procedure {name = "fun3", arguments = [("ref1",Ref TUInt64)], body = [GetRef (Fresh "v0") TUInt64 (Dynamic ("ref1",Ref TUInt64)),After (Lit TUInt64 (LUInt64 477)) (Dynamic ("ref1",Ref TUInt64)) (BOp TUInt64 (BOp TUInt64 (Var TUInt64 "v0") (Var TUInt64 "v0") OTimes) (BOp TUInt64 (Lit TUInt64 (LUInt64 11486)) (Lit TUInt64 (LUInt64 56986)) OTimes) OPlus),Fork [("fun4",[Right $ Dynamic ("ref1",Ref TUInt64)])],Wait [Dynamic ("ref1",Ref TUInt64)],Fork [("fun4",[Right $ Dynamic ("ref1",Ref TUInt64)])]]}),("fun4",Procedure {name = "fun4", arguments = [("ref1",Ref TUInt64)], body = [Wait [Dynamic ("ref1",Ref TUInt64)],After (Lit TUInt64 (LUInt64 4364)) (Dynamic ("ref1",Ref TUInt64)) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 63649)) (Lit TUInt64 (LUInt64 1)) OMinus) (BOp TUInt64 (Lit TUInt64 (LUInt64 1)) (Lit TUInt64 (LUInt64 964)) OPlus) OMinus),Fork [("fun4",[Right $ Dynamic ("ref1",Ref TUInt64)]),("fun4",[Right $ Dynamic ("ref1",Ref TUInt64)]),("fun4",[Right $ Dynamic ("ref1",Ref TUInt64)]),("fun4",[Right $ Dynamic ("ref1",Ref TUInt64)]),("fun4",[Right $ Dynamic ("ref1",Ref TUInt64)])]]})]}

spec :: H.Spec
spec = T.doProgramSpec "Test1623691869727" p
