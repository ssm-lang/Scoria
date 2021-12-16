{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.SynthesizeDelaySpec where

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

fun0 :: SSM ()
fun0 = routine $ do
    fork [ do wake <- var event
              after (secs 1) wake event
              wait wake
         ]

p1 :: Compile backend ()
p1 = schedule fun0

p :: Program backend
p = Program
  {
      initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
    , funs = fromList [( Ident "generated0" Nothing
                       , Procedure { name = Ident "generated0" Nothing
                                   , arguments = []
                                   , body = [ NewRef (Ident "var0" Nothing) TEvent (Lit TEvent LEvent)
                                            , After (BOp TUInt64 (Lit TUInt64 (LUInt64 1)) (Lit TUInt64 (LUInt64 1000000000)) OTimes) (Dynamic (Ident "var0" Nothing,Ref TEvent)) (Lit TEvent LEvent)
                                            , Wait [Dynamic (Ident "var0" Nothing,Ref TEvent)]]})
                       ,( Ident "fun0" Nothing
                        , Procedure (Ident "fun0" Nothing) [] [Fork [(Ident "generated0" Nothing,[])]]
                        )
                       ]
    , peripherals = []}


spec :: H.Spec
spec = T.propSyntacticEquality "SynthesizeDelay" (toProgram p1) p
