module Regression.LaterAssignOverwriteSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = Ident "fun1" Nothing
  , args  = [Right (Ident "ref1" Nothing, Ref TBool), Right (Ident "ref3" Nothing, Ref TInt32)]
  , funs  = fromList
              [ ( Ident "fun1" Nothing
                , Procedure
                  { name = Ident "fun1" Nothing
                  , arguments = [(Ident "ref1" Nothing, Ref TBool), (Ident "ref3" Nothing, Ref TInt32)]
                  , body = [ After (Lit TUInt64 (LUInt64 2))
                                   (Ident "ref1" Nothing, Ref TBool)
                                   (Lit TBool (LBool True))
                           , After (Lit TUInt64 (LUInt64 1))
                                   (Ident "ref3" Nothing, Ref TInt32)
                                   (Lit TInt32 (LInt32 3))
                           , SetRef (Ident "ref3" Nothing, Ref TInt32) (Lit TInt32 (LInt32 4))
                           , Wait [(Ident "ref1" Nothing, Ref TBool)]
                           , Wait [(Ident "ref3" Nothing, Ref TInt32)]
                           ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.correctSpec "LaterAssignOverwriteSpec" p
