-- | Schedule a delayed assignment, but leave immediately.
module Regression.CancelLaterSpec where

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
                  { name = "fun0"
                  , arguments = []
                  , body = [ NewRef (Fresh "v0")
                                    (Ref TInt32)
                                    (Lit TInt32 (LInt32 0))
                           , After (Lit TUInt64 (LUInt64 2))
                                   ("v0", Ref TInt32)
                                   (Lit TInt32 (LInt32 1))
                           ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.correctSpec "CancelLater" p
