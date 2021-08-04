-- | Recurse forever.
--
-- We should not run out of depth, because that should only be incremented if
-- forking multiple children.
--
-- This is the front-end version of regression-low/Regression/RecurseForeverSpec.hs
module Regression.RecurseForeverSpec where

import           SSM.Frontend.Language
import           SSM.Frontend.Box
import           SSM.Frontend.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

fun0 :: SSM ()
fun0 = boxNullary "fun0" $ do
    fork [ fun0 ]

spec :: H.Spec
spec = T.correctSpec "RecurseForever" fun0
