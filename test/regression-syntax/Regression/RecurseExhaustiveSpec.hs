{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.RecurseExhaustiveSpec where

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
fun0 = routine $ fork [fun0, fun1]

fun1 :: SSM ()
fun1 = routine $ return ()

p :: Program
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs  = fromList
              [ ( Ident "fun0" Nothing
                , Procedure { name      = Ident "fun0" Nothing
                            , arguments = []
                            , body      = [Fork [(Ident "fun0" Nothing, []), (Ident "fun1" Nothing, [])]]
                            }
                )
              , (Ident "fun1" Nothing, Procedure { name = Ident "fun1" Nothing, arguments = [], body = [] })
              ]
  , peripherals = []}

spec :: H.Spec
spec = T.propSyntacticEquality "RecurseExhaustive" fun0 p
