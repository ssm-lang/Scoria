module Regression.LaterAssignOverwriteSpec where

import           Data.Map                       ( fromList )
import           LowCore
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.Ssm.Prop                 as T

p :: Program
p = Program
  { entry = "fun1"
  , args  = [Right ("ref1", Ref TBool), Right ("ref3", Ref TInt32)]
  , funs  = fromList
              [ ( "fun1"
                , Procedure
                  { name = "fun1"
                  , arguments = [("ref1", Ref TBool), ("ref3", Ref TInt32)]
                  , body = [ After (Lit TUInt64 (LUInt64 2))
                                   ("ref1", Ref TBool)
                                   (Lit TBool (LBool True))
                           , After (Lit TUInt64 (LUInt64 1))
                                   ("ref3", Ref TInt32)
                                   (Lit TInt32 (LInt32 3))
                           , SetRef ("ref3", Ref TInt32) (Lit TInt32 (LInt32 4))
                           , Wait [("ref1", Ref TBool)]
                           , Wait [("ref3", Ref TInt32)]
                           ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.doProgramSpec "LaterAssignOverwriteSpec" p
