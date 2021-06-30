module Regression.Test1624511150452Spec where

import Data.Map (fromList)
import LowCore
import qualified Test.Ssm.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = "fun1", args = [Right ("ref6",Ref TInt32),Right ("ref8",Ref TUInt64)], funs = fromList [("fun1",Procedure {name = "fun1", arguments = [("ref6",Ref TInt32),("ref8",Ref TUInt64)], body = [NewRef (Fresh "v0") (Ref TInt32) (UOpE TInt32 (Lit TInt32 (LInt32 111)) Neg),NewRef (Fresh "v1") (Ref TInt32) (BOp TInt32 (UOpE TInt32 (Lit TInt32 (LInt32 1)) Neg) (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OMinus) OMinus),After (Lit TUInt64 (LUInt64 4013)) ("ref6",Ref TInt32) (UOpE TInt32 (BOp TInt32 (UOpE TInt32 (Lit TInt32 (LInt32 1)) Neg) (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OTimes) OTimes) Neg),Wait [("ref6",Ref TInt32)],If (UOpR TBool ("ref6",Ref TInt32) Changed) [NewRef (Fresh "v2") (Ref TBool) (BOp TBool (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OEQ)] [Wait [("ref8",Ref TUInt64)]],NewRef (Fresh "v3") (Ref TInt32) (UOpE TInt32 (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OMinus) Neg),After (Lit TUInt64 (LUInt64 38)) ("v0",Ref TInt32) (BOp TInt32 (UOpE TInt32 (Lit TInt32 (LInt32 1)) Neg) (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OTimes) OTimes),GetRef (Fresh "v6") TInt32 ("ref6",Ref TInt32),GetRef (Fresh "v7") TUInt64 ("ref8",Ref TUInt64),SetRef ("ref8",Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Var TUInt64 "v7") (Var TUInt64 "v7") OMinus) (BOp TUInt64 (Lit TUInt64 (LUInt64 3028)) (Lit TUInt64 (LUInt64 52275)) OPlus) OMinus)]})]}

spec :: H.Spec
spec = T.correctSpec "Test1624511150452" p
