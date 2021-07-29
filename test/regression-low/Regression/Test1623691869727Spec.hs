module Regression.Test1623691869727Spec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = Ident "fun3" Nothing, args = [Right (Ident "ref1" Nothing,Ref TUInt64)], funs = fromList [(Ident "fun3" Nothing,Procedure {name = Ident "fun3" Nothing, arguments = [(Ident "ref1" Nothing,Ref TUInt64)], body = [GetRef ((Ident "v0" Nothing)) TUInt64 (Ident "ref1" Nothing,Ref TUInt64),After (Lit TUInt64 (LUInt64 477)) (Ident "ref1" Nothing,Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Var TUInt64 (Ident "v0" Nothing)) (Var TUInt64 (Ident "v0" Nothing)) OTimes) (BOp TUInt64 (Lit TUInt64 (LUInt64 11486)) (Lit TUInt64 (LUInt64 56986)) OTimes) OPlus),Fork [(Ident "fun4" Nothing,[Right (Ident "ref1" Nothing,Ref TUInt64)])],Wait [(Ident "ref1" Nothing,Ref TUInt64)],Fork [(Ident "fun4" Nothing,[Right (Ident "ref1" Nothing,Ref TUInt64)])]]}),(Ident "fun4" Nothing,Procedure {name = Ident "fun4" Nothing, arguments = [(Ident "ref1" Nothing,Ref TUInt64)], body = [Wait [(Ident "ref1" Nothing,Ref TUInt64)],After (Lit TUInt64 (LUInt64 4364)) (Ident "ref1" Nothing,Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 63649)) (Lit TUInt64 (LUInt64 1)) OMinus) (BOp TUInt64 (Lit TUInt64 (LUInt64 1)) (Lit TUInt64 (LUInt64 964)) OPlus) OMinus),Fork [(Ident "fun4" Nothing,[Right (Ident "ref1" Nothing,Ref TUInt64)]),(Ident "fun4" Nothing,[Right (Ident "ref1" Nothing,Ref TUInt64)]),(Ident "fun4" Nothing,[Right (Ident "ref1" Nothing,Ref TUInt64)]),(Ident "fun4" Nothing,[Right (Ident "ref1" Nothing,Ref TUInt64)]),(Ident "fun4" Nothing,[Right (Ident "ref1" Nothing,Ref TUInt64)])]]})]}

spec :: H.Spec
spec = T.correctSpec "Test1623691869727" p
