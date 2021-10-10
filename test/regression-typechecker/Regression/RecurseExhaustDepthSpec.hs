-- | Recurse forever, until depth is exhausted.
module Regression.RecurseExhaustDepthSpec where

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
                , Procedure { name      = Ident "fun0" Nothing
                            , arguments = []
                            , body      = [Fork [(Ident "fun0" Nothing, []), (Ident "fun1" Nothing, [])]]
                            }
                )
              , (Ident "fun1" Nothing, Procedure { name = Ident "fun1" Nothing, arguments = [], body = [] })
              ]
  , peripherals = []}

spec :: H.Spec
spec = T.correctSpec "RecurseExhaustDepth" p
