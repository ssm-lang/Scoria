module Regression.Test1624511150452Spec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = "fun1", args = [Right $ Dynamic ("ref6",Ref TInt32),Right $ Dynamic ("ref8",Ref TUInt64)], funs = fromList [("fun1",Procedure {name = "fun1", arguments = [("ref6",Ref TInt32),("ref8",Ref TUInt64)], body = [NewRef (Fresh "v0") TInt32 (UOpE TInt32 (Lit TInt32 (LInt32 111)) Neg),NewRef (Fresh "v1") TInt32 (BOp TInt32 (UOpE TInt32 (Lit TInt32 (LInt32 1)) Neg) (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OMinus) OMinus),After (Lit TUInt64 (LUInt64 4013)) (Dynamic ("ref6",Ref TInt32)) (UOpE TInt32 (BOp TInt32 (UOpE TInt32 (Lit TInt32 (LInt32 1)) Neg) (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OTimes) OTimes) Neg),Wait [Dynamic ("ref6",Ref TInt32)],If (UOpR TBool (Dynamic ("ref6",Ref TInt32)) Changed) [NewRef (Fresh "v2") TBool (BOp TBool (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OEQ)] [Wait [Dynamic ("ref8",Ref TUInt64)]],NewRef (Fresh "v3") TInt32 (UOpE TInt32 (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OMinus) Neg),After (Lit TUInt64 (LUInt64 38)) (Dynamic ("v0",Ref TInt32)) (BOp TInt32 (UOpE TInt32 (Lit TInt32 (LInt32 1)) Neg) (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OTimes) OTimes),GetRef (Fresh "v6") TInt32 (Dynamic ("ref6",Ref TInt32)),GetRef (Fresh "v7") TUInt64 (Dynamic ("ref8",Ref TUInt64)),SetRef (Dynamic ("ref8",Ref TUInt64)) (BOp TUInt64 (BOp TUInt64 (Var TUInt64 "v7") (Var TUInt64 "v7") OMinus) (BOp TUInt64 (Lit TUInt64 (LUInt64 3028)) (Lit TUInt64 (LUInt64 52275)) OPlus) OMinus)]})], global_references = []}

spec :: H.Spec
spec = T.correctSpec "Test1624511150452" p
