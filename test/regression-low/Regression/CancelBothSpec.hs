-- | Simple test case that schedules two delayed assignments, but leaves before
-- peforming either.
module Regression.CancelBothSpec where

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
          [ NewRef (Fresh "v0") (Ref TBool) (Lit TBool (LBool False))
          , After (Lit TUInt64 (LUInt64 1))
                  ("v0", Ref TBool)
                  (Lit TBool (LBool True))
          , NewRef (Fresh "v1")
                   (Ref TBool)
                   (UOpR TBool ("v0", Ref TBool) Changed)
          , After (Lit TUInt64 (LUInt64 3872))
                  ("v1", Ref TBool)
                  (Lit TBool (LBool False))
          ]
        }
      )
    ]
  }

spec :: H.Spec
spec = T.correctSpec "CancelBoth" p
