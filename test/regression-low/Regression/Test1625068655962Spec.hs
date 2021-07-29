module Regression.Test1625068655962Spec where

import Data.Map (fromList)
import SSM.Core.Syntax
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

p :: Program
p = Program {entry = (Ident "fun2" Nothing), args = [Right ((Ident "ref4" Nothing),Ref TInt32)], funs = fromList [((Ident "fun2" Nothing),Procedure {name = (Ident "fun2" Nothing), arguments = [((Ident "ref4" Nothing),Ref TInt32)], body = [Wait [((Ident "ref4" Nothing),Ref TInt32)],Wait [((Ident "ref4" Nothing),Ref TInt32)],Fork [((Ident "fun2" Nothing),[Right ((Ident "ref4" Nothing),Ref TInt32)])]]})]}

spec :: H.Spec
spec = T.correctSpec "Test1625068655962" p
