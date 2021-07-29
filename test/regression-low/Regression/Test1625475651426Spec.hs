module Regression.Test1625475651426Spec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = (Ident "fun1" Nothing), args = [Right ((Ident "ref1" Nothing),Ref TBool)], funs = fromList [((Ident "fun1" Nothing),Procedure {name = (Ident "fun1" Nothing), arguments = [((Ident "ref1" Nothing),Ref TBool)], body = [SetRef ((Ident "ref1" Nothing),Ref TBool) (Lit TBool (LBool True)),Wait [((Ident "ref1" Nothing),Ref TBool)],After (Lit TUInt64 (LUInt64 3065)) ((Ident "ref1" Nothing),Ref TBool) (BOp TBool (BOp TInt32 (Lit TInt32 (LInt32 23)) (Lit TInt32 (LInt32 71)) OTimes) (BOp TInt32 (Lit TInt32 (LInt32 176)) (Lit TInt32 (LInt32 99)) OPlus) OLT),NewRef ((Ident "v2" Nothing)) (Ref TBool) (BOp TBool (BOp TBool (Lit TInt32 (LInt32 117)) (Lit TInt32 (LInt32 73)) OLT) (BOp TBool (Lit TBool (LBool False)) (Lit TBool (LBool True)) OEQ) OEQ),Fork [((Ident "fun1" Nothing),[Right ((Ident "ref1" Nothing),Ref TBool)])],Wait [((Ident "v2" Nothing),Ref TBool)],Wait [((Ident "ref1" Nothing),Ref TBool)],Fork [((Ident "fun1" Nothing),[Right ((Ident "ref1" Nothing),Ref TBool)])],After (Lit TUInt64 (LUInt64 1338)) ((Ident "ref1" Nothing),Ref TBool) (BOp TBool (Lit TBool (LBool True)) (Lit TBool (LBool True)) OEQ),Fork [((Ident "fun1" Nothing),[Right ((Ident "v2" Nothing),Ref TBool)])],Wait [((Ident "ref1" Nothing),Ref TBool)],Wait [((Ident "ref1" Nothing),Ref TBool)],Fork [((Ident "fun1" Nothing),[Right ((Ident "v2" Nothing),Ref TBool)])]]})]}

spec :: H.Spec
spec = T.correctSpec "Test1625475651426" p
