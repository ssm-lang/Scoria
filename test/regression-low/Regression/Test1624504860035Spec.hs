module Regression.Test1624504860035Spec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = "fun1", args = [Right $ Dynamic ("ref5",Ref TInt64)], funs = fromList [("fun1",Procedure {name = "fun1", arguments = [("ref5",Ref TInt64)], body = [NewRef (Fresh "v1") TInt32 (Lit TInt32 (LInt32 125)),If (UOpR TBool (Dynamic ("ref5",Ref TInt64)) Changed) [SetRef (Dynamic ("ref5",Ref TInt64)) (BOp TInt64 (BOp TInt64 (Lit TInt64 (LInt64 1)) (Lit TInt64 (LInt64 1)) OPlus) (BOp TInt64 (Lit TInt64 (LInt64 (-49462))) (Lit TInt64 (LInt64 (-34759))) OTimes) OPlus),Wait [Dynamic ("v1",Ref TInt32)],GetRef (Fresh "v2") TInt32 (Dynamic ("v1",Ref TInt32)),NewRef (Fresh "v4") TBool (BOp TBool (BOp TInt32 (Lit TInt32 (LInt32 165)) (Lit TInt32 (LInt32 173)) OPlus) (BOp TInt32 (Var TInt32 "v2") (Lit TInt32 (LInt32 111)) OTimes) OEQ)] [If (BOp TBool (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OEQ) [Wait [Dynamic ("v1",Ref TInt32)]] [If (BOp TBool (BOp TInt32 (Lit TInt32 (LInt32 213)) (Lit TInt32 (LInt32 163)) OPlus) (UOpE TInt32 (Lit TInt32 (LInt32 1)) Neg) OEQ) [] [Wait [Dynamic ("v1",Ref TInt32),Dynamic ("ref5",Ref TInt64)]],If (BOp TBool (BOp TBool (Lit TInt32 (LInt32 91)) (Lit TInt32 (LInt32 1)) OLT) (BOp TBool (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OEQ) OEQ) [] []],After (Lit TUInt64 (LUInt64 3414)) (Dynamic ("v1",Ref TInt32)) (BOp TInt32 (BOp TInt32 (Lit TInt32 (LInt32 157)) (Lit TInt32 (LInt32 8)) OTimes) (BOp TInt32 (Lit TInt32 (LInt32 39)) (Lit TInt32 (LInt32 1)) OPlus) OMinus),NewRef (Fresh "v5") TBool (BOp TBool (Lit TInt32 (LInt32 191)) (Lit TInt32 (LInt32 141)) OEQ),After (Lit TUInt64 (LUInt64 3980)) (Dynamic ("v5",Ref TBool)) (BOp TBool (Lit TInt32 (LInt32 182)) (Lit TInt32 (LInt32 1)) OEQ)],Wait [Dynamic ("ref5",Ref TInt64)],After (Lit TUInt64 (LUInt64 820)) (Dynamic ("ref5",Ref TInt64)) (BOp TInt64 (Lit TInt64 (LInt64 1)) (Lit TInt64 (LInt64 1)) OPlus),After (Lit TUInt64 (LUInt64 2945)) (Dynamic ("v1",Ref TInt32)) (BOp TInt32 (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OTimes) (BOp TInt32 (Lit TInt32 (LInt32 182)) (Lit TInt32 (LInt32 1)) OPlus) OPlus),NewRef (Fresh "v6") TInt32 (BOp TInt32 (Lit TInt32 (LInt32 169)) (Lit TInt32 (LInt32 1)) OTimes),Wait [Dynamic ("ref5",Ref TInt64)],Wait [Dynamic ("ref5",Ref TInt64)],Wait [Dynamic ("ref5",Ref TInt64)]]})], global_references = []}

spec :: H.Spec
spec = T.correctSpec "Test1624504860035" p
