module Regression.Test1625480635361Spec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = Ident "fun2" Nothing, args = [Right (Ident "ref2" Nothing,Ref TUInt64)], funs = fromList [(Ident "fun2" Nothing,Procedure {name = Ident "fun2" Nothing, arguments = [(Ident "ref2" Nothing,Ref TUInt64)], body = [Fork [(Ident "fun5" Nothing,[Right (Ident "ref2" Nothing,Ref TUInt64)])],Wait [(Ident "ref2" Nothing,Ref TUInt64)],GetRef ((Ident "v0" Nothing)) TUInt64 (Ident "ref2" Nothing,Ref TUInt64),GetRef ((Ident "v1" Nothing)) TUInt64 (Ident "ref2" Nothing,Ref TUInt64)]}),(Ident "fun5" Nothing,Procedure {name = Ident "fun5" Nothing, arguments = [(Ident "ref6" Nothing,Ref TUInt64)], body = [SetRef (Ident "ref6" Nothing,Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 48176)) (Lit TUInt64 (LUInt64 17986)) OMinus) (BOp TUInt64 (Lit TUInt64 (LUInt64 57880)) (Lit TUInt64 (LUInt64 33673)) OTimes) OMinus),Wait [(Ident "ref6" Nothing,Ref TUInt64)],After (Lit TUInt64 (LUInt64 1934)) (Ident "ref6" Nothing,Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 1)) (Lit TUInt64 (LUInt64 1)) OTimes) (BOp TUInt64 (Lit TUInt64 (LUInt64 1)) (Lit TUInt64 (LUInt64 31717)) OTimes) OPlus),Fork [(Ident "fun5" Nothing,[Right (Ident "ref6" Nothing,Ref TUInt64)])]]})]}

spec :: H.Spec
spec = T.correctSpec "Test1625480635361" p
