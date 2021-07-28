module Regression.Test1625475651426Spec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = "fun1", args = [Right $ Dynamic ("ref1",Ref TBool)], funs = fromList [("fun1",Procedure {name = "fun1", arguments = [("ref1",Ref TBool)], body = [SetRef (Dynamic ("ref1",Ref TBool)) (Lit TBool (LBool True)),Wait [Dynamic $ ("ref1",Ref TBool)],After (Lit TUInt64 (LUInt64 3065)) (Dynamic ("ref1",Ref TBool)) (BOp TBool (BOp TInt32 (Lit TInt32 (LInt32 23)) (Lit TInt32 (LInt32 71)) OTimes) (BOp TInt32 (Lit TInt32 (LInt32 176)) (Lit TInt32 (LInt32 99)) OPlus) OLT),NewRef (Fresh "v2") TBool (BOp TBool (BOp TBool (Lit TInt32 (LInt32 117)) (Lit TInt32 (LInt32 73)) OLT) (BOp TBool (Lit TBool (LBool False)) (Lit TBool (LBool True)) OEQ) OEQ),Fork [("fun1",[Right $ Dynamic ("ref1",Ref TBool)])],Wait [Dynamic $ ("v2",Ref TBool)],Wait [Dynamic $ ("ref1",Ref TBool)],Fork [("fun1",[Right $ Dynamic ("ref1",Ref TBool)])],After (Lit TUInt64 (LUInt64 1338)) (Dynamic ("ref1",Ref TBool)) (BOp TBool (Lit TBool (LBool True)) (Lit TBool (LBool True)) OEQ),Fork [("fun1",[Right $ Dynamic ("v2",Ref TBool)])],Wait [Dynamic $ ("ref1",Ref TBool)],Wait [Dynamic $ ("ref1",Ref TBool)],Fork [("fun1",[Right $ Dynamic ("v2",Ref TBool)])]]})], global_references = []}

spec :: H.Spec
spec = T.correctSpec "Test1625475651426" p
