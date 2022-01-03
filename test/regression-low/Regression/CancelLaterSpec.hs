-- | Schedule a delayed assignment, but leave immediately.
module Regression.CancelLaterSpec where

import           Data.Map                       ( fromList )
import           SSM.Core
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program backend
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs  = fromList
              [ ( Ident "fun0" Nothing
                , Procedure
                  { name = Ident "fun0" Nothing
                  , arguments = []
                  , body = [ NewRef (Ident "v0" Nothing)
                                    TInt32
                                    (Lit TInt32 (LInt32 0))
                           , After (Lit TUInt64 (LUInt64 2))
                                   (Dynamic (Ident "v0" Nothing, Ref TInt32))
                                   (Lit TInt32 (LInt32 1))
                           ]
                  }
                )
              ]
  , peripherals = []}

spec :: H.Spec
spec = T.correctSpec "CancelLater" p
