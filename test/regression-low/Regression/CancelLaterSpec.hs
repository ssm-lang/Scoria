-- | Schedule a delayed assignment, but leave immediately.
module Regression.CancelLaterSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = Ident "fun0" Nothing
  , funs  = fromList
              [ ( Ident "fun0" Nothing
                , Procedure
                  { name = Ident "fun0" Nothing
                  , arguments = []
                  , body = [ CreateRef (Ident "v0" Nothing) (Ref TInt32)
                           , SetRef (Dynamic (Ident "v0" Nothing, Ref TInt32)) (Lit TInt32 (LInt32 0))
                           , After (SSMTime (Lit TUInt64 (LUInt64 2))
                                            SSMNanosecond)
                                   (Dynamic (Ident "v0" Nothing, Ref TInt32))
                                   (Lit TInt32 (LInt32 1))
                           ]
                  }
                )
              ]
  , globalReferences = []}

spec :: H.Spec
spec = T.correctSpec "CancelLater" p
