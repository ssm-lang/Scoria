-- | Simple test case that schedules two delayed assignments, but leaves before
-- peforming either.
module Regression.CancelBothSpec where

import           Data.Map                       ( fromList )
import           SSM.Core
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

import SSM.Compile
import SSM.Interpret

p :: Program backend
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs  = fromList
    [ ( Ident "fun0" Nothing
      , Procedure
        { name      = Ident "fun0" Nothing
        , arguments = []
        , body      =
          [ NewRef (Ident "v0" Nothing) TBool (Lit TBool (LBool False))
          , After (Lit TUInt64 (LUInt64 1))
                  (Dynamic (Ident "v0" Nothing, Ref TBool))
                  (Lit TBool (LBool True))
          , NewRef (Ident "v1" Nothing)
                   TBool
                   (UOpR TBool (Dynamic (Ident "v0" Nothing, Ref TBool)) Changed)
          , After (Lit TUInt64 (LUInt64 3872))
                  (Dynamic (Ident "v1" Nothing, Ref TBool))
                  (Lit TBool (LBool False))
          ]
        }
      )
    ]
  , peripherals = []}

spec :: H.Spec
spec = T.correctSpec "CancelBoth" p
