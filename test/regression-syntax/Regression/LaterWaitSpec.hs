{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.LaterWaitSpec where

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
    fresh0 <- var (0 :: Exp Int32)
    after (nsecs 2) fresh0 1
    wait fresh0

p :: Program
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs  = fromList
    [ ( Ident "fun0" Nothing
      , Procedure
        { name = Ident "fun0" Nothing
        , arguments = []
        , body = [ NewRef (Ident "fresh0" Nothing) TInt32 (Lit TInt32 (LInt32 0))
                 , After (Lit TUInt64 (LUInt64 2))
                         (Dynamic (Ident "fresh0" Nothing, Ref TInt32))
                         (Lit TInt32 (LInt32 1))
                 , Wait [Dynamic (Ident "fresh0" Nothing, Ref TInt32)]
                 ]
        }
      )
    ]
  , peripherals = []}

spec :: H.Spec
spec = T.propSyntacticEquality "LaterWait" fun0 p
