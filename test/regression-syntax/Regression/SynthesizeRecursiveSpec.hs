{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.SynthesizeRecursiveSpec where

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
    x <- var event'
    fork [ do wait x
              fork [wait x]
         ]

p = Program
  {
      initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
    , funs = fromList [( Ident "fun0" Nothing
                        , Procedure (Ident "fun0" Nothing) []
                            [ NewRef (Ident "fresh0" Nothing) TEvent (Lit TEvent LEvent)
                            , Fork [(Ident "generatedfresh0" Nothing,[Right $ Dynamic (Ident "fresh0" Nothing, Ref TEvent)])]]
                       )
                      , ( Ident "generatedfresh0" Nothing
                        , Procedure (Ident "generatedfresh0" Nothing)
                            [(Ident "fresh0" Nothing, Ref TEvent)]
                            [ Wait [Dynamic (Ident "fresh0" Nothing, Ref TEvent)]
                            , Fork [(Ident "generatedfresh1" Nothing, [Right $ Dynamic (Ident "fresh0" Nothing, Ref TEvent)])]
                          ]
                        )
                      , ( Ident "generatedfresh1" Nothing
                        , Procedure (Ident "generatedfresh1" Nothing)
                            [(Ident "fresh0" Nothing, Ref TEvent)]
                            [Wait [Dynamic (Ident "fresh0" Nothing, Ref TEvent)]]
                        )
                      ]
    , peripherals = []}


spec :: H.Spec
spec = T.propSyntacticEquality "SynthesizeRecursive" fun0 p
