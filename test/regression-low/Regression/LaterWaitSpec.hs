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
  { entry = Ident "fun0" Nothing
  , args  = []
  , funs  = fromList
    [ ( Ident "fun0" Nothing
      , Procedure
        { name = Ident "fun0" Nothing
        , arguments = []
        , body = [ NewRef (Ident "v0" Nothing) (Ref TInt32) (Lit TInt32 (LInt32 0))
                 , After (Lit TUInt64 (LUInt64 2))
                         (Ident "v0" Nothing, Ref TInt32)
                         (Lit TInt32 (LInt32 1))
                 , GetRef (Ident "v3" Nothing) TInt32 (Ident "v0" Nothing, Ref TInt32)
                 , Wait [(Ident "v0" Nothing, Ref TInt32)]
                 ]
        }
      )
    ]
  }
