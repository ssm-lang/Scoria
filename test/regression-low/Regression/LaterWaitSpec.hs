-- | Schedule a delayed assignment to a variable, and wait on it.
--
-- This example was created and saved upon provoking a bug pretty much unrelated
-- to what it's doing. But it's still a good example.
--
-- Bug encountered: Local variables' values were correctly reported, but the
-- order was wrong.
-- Cause: In the interpreter, GetRef used to add the new variable to the
-- variables list (for arguments) rather than the localrefs list.
-- Fix: Fix that function.
-- Fixed: fd51ef1
module Regression.LaterWaitSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

spec :: H.Spec
spec = T.correctSpec "LaterWaitSpec" p

p :: Program
p = Program
  { entry = "fun0"
  , args  = []
  , funs  = fromList
    [ ( "fun0"
      , Procedure
        { name = "fun0"
        , arguments = []
        , body = [ NewRef (Fresh "v0") (Ref TInt32) (Lit TInt32 (LInt32 0))
                 , After (Lit TUInt64 (LUInt64 2))
                         ("v0", Ref TInt32)
                         (Lit TInt32 (LInt32 1))
                 , GetRef (Fresh "v3") TInt32 ("v0", Ref TInt32)
                 , Wait [("v0", Ref TInt32)]
                 ]
        }
      )
    ]
  }
