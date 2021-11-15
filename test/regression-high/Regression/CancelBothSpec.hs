{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
module Regression.CancelBothSpec where

import           Prelude hiding (sum)

import           Data.Word
import           SSM.Frontend.Language
import           SSM.Frontend.Box
import           SSM.Frontend.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T


fun0 :: SSM ()
fun0 = routine $ do
    v0 <- var false'
    after (nsecs 1) v0 true'
    v1 <- var $ changed v0
    after (nsecs 3872) v1 false'

spec :: H.Spec
spec = T.correctSpec "CancelBoth" fun0
