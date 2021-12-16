{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.SynthesizeRecursiveSpec where

import           Prelude hiding (sum)

import SSM.Language
import SSM.Frontend.Peripheral.Identity
import SSM.Frontend.Compile hiding ( initialQueueContent, peripherals )
import SSM.Core
import Data.Map as Map
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T
import Data.Int

p1 :: Compile backend ()
p1 = schedule fun0

fun0 :: SSM ()
fun0 = routine $ do
    x <- var event
    fork [ do wait x
              fork [wait x]
         ]

p :: Program backend
p = Program
  {
      initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
    , funs = fromList [( Ident "fun0" Nothing
                        , Procedure (Ident "fun0" Nothing) []
                            [ NewRef (Ident "var0" Nothing) TEvent (Lit TEvent LEvent)
                            , Fork [(Ident "generated0" Nothing,[Right $ Dynamic (Ident "var0" Nothing, Ref TEvent)])]]
                       )
                      , ( Ident "generated0" Nothing
                        , Procedure (Ident "generated0" Nothing)
                            [(Ident "var0" Nothing, Ref TEvent)]
                            [ Wait [Dynamic (Ident "var0" Nothing, Ref TEvent)]
                            , Fork [(Ident "generated1" Nothing, [Right $ Dynamic (Ident "var0" Nothing, Ref TEvent)])]
                          ]
                        )
                      , ( Ident "generated1" Nothing
                        , Procedure (Ident "generated1" Nothing)
                            [(Ident "var0" Nothing, Ref TEvent)]
                            [Wait [Dynamic (Ident "var0" Nothing, Ref TEvent)]]
                        )
                      ]
    , peripherals = []}


spec :: H.Spec
spec = T.propSyntacticEquality "SynthesizeRecursive" (toProgram p1) p
