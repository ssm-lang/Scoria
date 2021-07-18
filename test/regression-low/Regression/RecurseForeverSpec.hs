-- | Recurse forever.
--
-- We should not run out of depth, because that should only be incremented if
-- forking multiple children.
module Regression.RecurseForeverSpec where

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
                  , body      = [ Fork [("fun0", [])] ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.correctSpec "RecurseForever" p
