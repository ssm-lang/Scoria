module Regression.Test1624511150452Spec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = Ident "fun1" Nothing, args = [Right (Ident "ref6" Nothing,Ref TInt32),Right (Ident "ref8" Nothing,Ref TUInt64)], funs = fromList [(Ident "fun1" Nothing,Procedure {name = Ident "fun1" Nothing, arguments = [(Ident "ref6" Nothing,Ref TInt32),(Ident "ref8" Nothing,Ref TUInt64)], body = [NewRef ( (Ident "v0" Nothing)) (Ref TInt32) (UOpE TInt32 (Lit TInt32 (LInt32 111)) Neg),NewRef ( (Ident "v1" Nothing)) (Ref TInt32) (BOp TInt32 (UOpE TInt32 (Lit TInt32 (LInt32 1)) Neg) (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OMinus) OMinus),After (Lit TUInt64 (LUInt64 4013)) (Ident "ref6" Nothing,Ref TInt32) (UOpE TInt32 (BOp TInt32 (UOpE TInt32 (Lit TInt32 (LInt32 1)) Neg) (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OTimes) OTimes) Neg),Wait [(Ident "ref6" Nothing,Ref TInt32)],If (UOpR TBool (Ident "ref6" Nothing,Ref TInt32) Changed) [NewRef ( (Ident "v2" Nothing)) (Ref TBool) (BOp TBool (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OEQ)] [Wait [(Ident "ref8" Nothing,Ref TUInt64)]],NewRef ( (Ident "v3" Nothing)) (Ref TInt32) (UOpE TInt32 (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OMinus) Neg),After (Lit TUInt64 (LUInt64 38)) ((Ident "v0" Nothing),Ref TInt32) (BOp TInt32 (UOpE TInt32 (Lit TInt32 (LInt32 1)) Neg) (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OTimes) OTimes),GetRef ( (Ident "v6" Nothing)) TInt32 (Ident "ref6" Nothing,Ref TInt32),GetRef ( (Ident "v7" Nothing)) TUInt64 (Ident "ref8" Nothing,Ref TUInt64),SetRef (Ident "ref8" Nothing,Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Var TUInt64 (Ident "v7" Nothing)) (Var TUInt64 (Ident "v7" Nothing)) OMinus) (BOp TUInt64 (Lit TUInt64 (LUInt64 3028)) (Lit TUInt64 (LUInt64 52275)) OPlus) OMinus)]})]}

spec :: H.Spec
spec = T.correctSpec "Test1624511150452" p
