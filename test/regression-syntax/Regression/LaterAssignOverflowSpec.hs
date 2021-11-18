{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.LaterAssignOverflowSpec where

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
    fresh0 <- var true'
    fresh1 <- var (0 :: Exp Int32)
    fork [fun1 fresh0 fresh1]

fun1 :: Ref Bool -> Ref Int32 -> SSM ()
fun1 ref1 ref3 = routine $ do
    after (nsecs 2) ref1 true'
    after (nsecs 1) ref3 3
    ref3 <~ 4
    wait ref1
    wait ref3

p :: Program
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs  = fromList
              [ ( Ident "fun0" Nothing
                , Procedure
                  { name      = Ident "fun0" Nothing
                  , arguments = []
                  , body      =
                    [ NewRef (Ident "fresh0" Nothing) TBool  (Lit TBool (LBool True))
                    , NewRef (Ident "fresh1" Nothing) TInt32 (Lit TInt32 (LInt32 0))
                    , Fork
                      [(Ident "fun1" Nothing, [Right (Dynamic (Ident "fresh0" Nothing, Ref TBool)), Right (Dynamic (Ident "fresh1" Nothing, Ref TInt32))])]
                    ]
                  }
                )
              , ( Ident "fun1" Nothing
                , Procedure
                  { name = Ident "fun1" Nothing
                  , arguments = [(Ident "ref1" Nothing, Ref TBool), (Ident "ref3" Nothing, Ref TInt32)]
                  , body = [ After (Lit TUInt64 (LUInt64 2))
                                   (Dynamic (Ident "ref1" Nothing, Ref TBool))
                                   (Lit TBool (LBool True))
                           , After (Lit TUInt64 (LUInt64 1))
                                   (Dynamic (Ident "ref3" Nothing, Ref TInt32))
                                   (Lit TInt32 (LInt32 3))
                           , SetRef (Dynamic (Ident "ref3" Nothing, Ref TInt32)) (Lit TInt32 (LInt32 4))
                           , Wait [Dynamic (Ident "ref1" Nothing, Ref TBool)]
                           , Wait [Dynamic (Ident "ref3" Nothing, Ref TInt32)]
                           ]
                  }
                )
              ]
  , peripherals = []
  }

spec :: H.Spec
spec = T.propSyntacticEquality "LaterAssign" fun0 p
