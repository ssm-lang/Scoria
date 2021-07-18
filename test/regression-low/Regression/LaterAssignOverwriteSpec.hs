-- | Whether an instantaneous assignment overwrites a previously scheduled
-- delayed assignment.
--
-- fun1 (r1: &bool, r3: &i32) =
--   after 2, r1 <- True
--   after 1, r3 <- 3
--   r3 <- 4
--   wait r1
--   wait r3
module Regression.LaterAssignOverwriteSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = "fun0"
  , args  = []
  , funs  = fromList
    [ ( "fun0"
      , Procedure
        { name      = "fun0"
        , arguments = []
        , body      =
          [ NewRef (Fresh "ref1") (Ref TBool)  (Lit TBool (LBool True))
          , NewRef (Fresh "ref3") (Ref TInt32) (Lit TInt32 (LInt32 0))
          , Fork
            [("fun1", [Right ("ref1", Ref TBool), Right ("ref3", Ref TInt32)])]
          ]
        }
      )
    , ( "fun1"
      , Procedure
        { name      = "fun1"
        , arguments = [("ref1", Ref TBool), ("ref3", Ref TInt32)]
        , body      = [ After (Lit TUInt64 (LUInt64 2))
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
spec = T.correctSpec "LaterAssignOverwriteSpec" p
