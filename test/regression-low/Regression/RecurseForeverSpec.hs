-- | Recurse forever.
--
-- We should not run out of depth, because that should only be incremented if
-- forking multiple children.
module Regression.RecurseForeverSpec where

import           Data.Map                       ( fromList )
import           SSM.Core
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs  = fromList
              [ ( Ident "fun0" Nothing
                , Procedure
                  { name      = Ident "fun0" Nothing
                  , arguments = []
                  , body      = [ Fork [(Ident "fun0" Nothing, [])] ]
                  }
                )
              ]
  , globalReferences = []
  , peripherals = []}

spec :: H.Spec
spec = T.correctSpec "RecurseForever" p
