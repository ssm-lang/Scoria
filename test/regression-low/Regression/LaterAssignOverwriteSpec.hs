module Regression.LaterAssignOverwriteSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = "fun1"
  , args  = [Right $ Dynamic ("ref1", Ref TBool), Right $ Dynamic ("ref3", Ref TInt32)]
  , funs  = fromList
              [ ( "fun1"
                , Procedure
                  { name = "fun1"
                  , arguments = [("ref1", Ref TBool), ("ref3", Ref TInt32)]
                  , body = [ After (Lit TUInt64 (LUInt64 2))
                                   (Dynamic ("ref1", Ref TBool))
                                   (Lit TBool (LBool True))
                           , After (Lit TUInt64 (LUInt64 1))
                                   (Dynamic ("ref3", Ref TInt32))
                                   (Lit TInt32 (LInt32 3))
                           , SetRef (Dynamic ("ref3", Ref TInt32)) (Lit TInt32 (LInt32 4))
                           , Wait [Dynamic ("ref1", Ref TBool)]
                           , Wait [Dynamic ("ref3", Ref TInt32)]
                           ]
                  }
                )
              ]
  , global_references = []
  }

spec :: H.Spec
spec = T.correctSpec "LaterAssignOverwriteSpec" p
