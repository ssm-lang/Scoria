module Regression.Test1624507133117Spec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = Ident "fun1" Nothing, args = [Right (Ident "ref1" Nothing,Ref TBool),Right (Ident "ref2" Nothing,Ref TUInt64)], funs = fromList [(Ident "fun1" Nothing,Procedure {name = Ident "fun1" Nothing, arguments = [(Ident "ref1" Nothing,Ref TBool),(Ident "ref2" Nothing,Ref TUInt64)], body = [Wait [(Ident "ref2" Nothing,Ref TUInt64)],After (Lit TUInt64 (LUInt64 2743)) (Ident "ref2" Nothing,Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 49181)) (Lit TUInt64 (LUInt64 27530)) OTimes) (BOp TUInt64 (Lit TUInt64 (LUInt64 3961)) (Lit TUInt64 (LUInt64 1352)) OTimes) OTimes),GetRef ((Ident "v1" Nothing)) TBool (Ident "ref1" Nothing,Ref TBool),After (Lit TUInt64 (LUInt64 195)) (Ident "ref2" Nothing,Ref TUInt64) (BOp TUInt64 (Lit TUInt64 (LUInt64 32467)) (Lit TUInt64 (LUInt64 53679)) OPlus),GetRef ((Ident "v4" Nothing)) TBool (Ident "ref1" Nothing,Ref TBool)]})]}

spec :: H.Spec
spec = T.correctSpec "Test1624507133117" p
