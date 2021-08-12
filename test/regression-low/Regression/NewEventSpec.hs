-- | Declare a new event-type variable.
--
-- Bug encountered: C compilation failed.
-- Cause: The SSM C library functions for event-type SVs take one fewer
-- argument. There's something wrong with the logic of how we generate calls to
-- those functions.
-- Fix: Fix that logic.
-- Fixed: notyet
module Regression.NewEventSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

spec :: H.Spec
spec = T.correctSpec "NewEvent" p

p :: Program
p = Program
  { entry            = Ident { identName = "fun0", identSrcInfo = Nothing }
  , funs             = fromList
                         [ ( Ident { identName = "fun0", identSrcInfo = Nothing }
                           , Procedure
                             { name = Ident { identName = "fun0", identSrcInfo = Nothing }
                             , arguments = []
                             , body = [ NewRef
                                          (Ident { identName    = "v0"
                                                 , identSrcInfo = Nothing
                                                 }
                                          )
                                          TEvent
                                          (Lit TEvent LEvent)
                                      ]
                             }
                           )
                         ]
  , globalReferences = []
  }
