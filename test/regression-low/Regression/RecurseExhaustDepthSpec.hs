-- | Recurse forever, until depth is exhausted.
module Regression.RecurseExhaustDepthSpec where

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
                , Procedure { name      = "fun0"
                            , arguments = []
                            , body      = [Fork [("fun0", []), ("fun1", [])]]
                            }
                )
              , ("fun1", Procedure { name = "fun1", arguments = [], body = [] })
              ]
  }

spec :: H.Spec
spec = T.correctSpec "RecurseExhaustDepth" p
