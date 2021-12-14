{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.SynthesizeDelaySpec where

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
    fork [ do wake <- var event'
              after (secs 1) wake event'
              wait wake
         ]

p = Program backend
  {
      initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
    , funs = fromList [( Ident "generatedfresh0" Nothing
                       , Procedure { name = Ident "generatedfresh0" Nothing
                                   , arguments = []
                                   , body = [ NewRef (Ident "fresh0" Nothing) TEvent (Lit TEvent LEvent)
                                            , After (BOp TUInt64 (Lit TUInt64 (LUInt64 1)) (Lit TUInt64 (LUInt64 1000000000)) OTimes) (Dynamic (Ident "fresh0" Nothing,Ref TEvent)) (Lit TEvent LEvent)
                                            , Wait [Dynamic (Ident "fresh0" Nothing,Ref TEvent)]]})
                       ,( Ident "fun0" Nothing
                        , Procedure (Ident "fun0" Nothing) [] [Fork [(Ident "generatedfresh0" Nothing,[])]]
                        )
                       ]
    , peripherals = []}


spec :: H.Spec
spec = T.propSyntacticEquality "SynthesizeDelay" fun0 p
