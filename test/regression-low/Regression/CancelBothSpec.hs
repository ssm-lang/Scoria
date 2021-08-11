-- | Simple test case that schedules two delayed assignments, but leaves before
-- peforming either.
module Regression.CancelBothSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

import SSM.Compile
import SSM.Interpret

p :: Program
p = Program
  { entry = Ident "fun0" Nothing
  , funs  = fromList
    [ ( Ident "fun0" Nothing
      , Procedure
        { name      = Ident "fun0" Nothing
        , arguments = []
        , body      =
          [ CreateRef (Ident "v0" Nothing) (Ref TBool)
          , SetRef (Dynamic (Ident "v0" Nothing, Ref TBool)) (Lit TBool (LBool False))
          , After (SSMTime (Lit TUInt64 (LUInt64 1)) (SSMNanosecond)
                  (Dynamic (Ident "v0" Nothing, Ref TBool))
                  (Lit TBool (LBool True))
          , CreateRef (Ident "v1" Nothing) (Ref TBool)
          , SetRef (Dynamic (Ident "v1" Nothing, Ref TBool)) (UOpR TBool (Dynamic (Ident "v0" Nothing, Ref TBool)) Changed)
          , After (SSMTime (Lit TUInt64 (LUInt64 3872)) SSMNanosecond)
                  (Dynamic (Ident "v1" Nothing, Ref TBool))
                  (Lit TBool (LBool False))
          ]
        }
      )
    ]
  , globalReferences = []}

spec :: H.Spec
spec = T.correctSpec "CancelBoth" p
