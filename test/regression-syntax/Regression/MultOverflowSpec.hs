{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.MultOverflowSpec where

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

fun1 :: SSM ()
fun1 = routine $ do
    fresh0 <- var (999999 :: Exp Int32)
    ifThenElse (0 <. (deref fresh0 * deref fresh0))
      (after (nsecs 2) fresh0 0)
      (after (nsecs 2) fresh0 1)
    wait fresh0

p1 :: Compile backend ()
p1 = schedule fun1

p :: Program backend
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun1" Nothing) []]
  , funs  = fromList
    [ ( Ident "fun1" Nothing
      , Procedure
        { name      = Ident "fun1" Nothing
        , arguments = []
        , body      =
          [ NewRef ((Ident "var0" Nothing)) TInt32 (Lit TInt32 (LInt32 999999))
          , If
            (BOp TBool
                 (Lit TInt32 (LInt32 0))
                 (BOp TInt32 (UOpR TInt32 (Dynamic (Ident "var0" Nothing, Ref TInt32)) Deref) (UOpR TInt32 (Dynamic (Ident "var0" Nothing, Ref TInt32)) Deref) OTimes)
                 OLT
            )
            [ After (Lit TUInt64 (LUInt64 2))
                    (Dynamic ((Ident "var0" Nothing), Ref TInt32))
                    (Lit TInt32 (LInt32 0))
            ]
            [ After (Lit TUInt64 (LUInt64 2))
                    (Dynamic ((Ident "var0" Nothing), Ref TInt32))
                    (Lit TInt32 (LInt32 1))
            ]
          , Wait [Dynamic ((Ident "var0" Nothing), Ref TInt32)]
          ]
        }
      )
    ]
  , peripherals = []}

spec :: H.Spec
spec = T.propSyntacticEquality "MultOverflow" (toProgram p1) p
