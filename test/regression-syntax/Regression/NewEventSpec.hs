{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.NewEventSpec where

import           Prelude hiding (sum)

import SSM.Language
import SSM.Frontend.Peripheral.Identity
import SSM.Core
import Data.Map as Map
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T
import Data.Int

fun0 :: SSM ()
fun0 = routine $ do
    var event
    return ()

p :: Program
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs             = fromList
                         [ ( Ident { identName = "fun0", identSrcInfo = Nothing }
                           , Procedure
                             { name = Ident { identName = "fun0", identSrcInfo = Nothing }
                             , arguments = []
                             , body = [ NewRef
                                          (Ident { identName    = "fresh0"
                                                 , identSrcInfo = Nothing
                                                 }
                                          )
                                          TEvent
                                          (Lit TEvent LEvent)
                                      ]
                             }
                           )
                         ]
  , peripherals = []
  }

spec :: H.Spec
spec = T.propSyntacticEquality "NewEvent" fun0 p
