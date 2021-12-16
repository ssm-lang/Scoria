{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
module Regression.FlipFlopLoopSpec where

import           Prelude hiding (sum)

import SSM.Frontend.Compile hiding ( initialQueueContent, peripherals )
import SSM.Language
import SSM.Core
import Data.Map as Map
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T
import Data.Int

fun0 :: SSM ()
fun0 = routine $ do
    ref2 <- var true
    fork [fun1 ref2]

fun1 :: Ref Bool -> SSM ()
fun1 fresh2 = routine $ do
    while true $ do
        after (nsecs 2) fresh2 false
        wait fresh2
        after (nsecs 2) fresh2 true
        wait fresh2

p1 :: Compile backend ()
p1 = schedule fun0

p :: Program backend
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs  = fromList
              [ ( Ident "fun0" Nothing
                , Procedure
                  { name = Ident "fun0" Nothing
                  , arguments = []
                  , body = [ NewRef (Ident "var0" Nothing) TBool (Lit TBool (LBool True))
                           , Fork [(Ident "fun1" Nothing, [Right (Dynamic (Ident "var0" Nothing, Ref TBool))])]
                           ]
                  }
                )
              , ( Ident "fun1" Nothing
                , Procedure
                  { name      = Ident "fun1" Nothing
                  , arguments = [(Ident "fresh2" Nothing, Ref TBool)]
                  , body      = [ While
                                    (Lit TBool (LBool True))
                                    [ After (Lit TUInt64 (LUInt64 2))
                                            (Dynamic (Ident "fresh2" Nothing, Ref TBool))
                                            (Lit TBool (LBool False))
                                    , Wait [Dynamic (Ident "fresh2" Nothing, Ref TBool)]
                                    , After (Lit TUInt64 (LUInt64 2))
                                            (Dynamic (Ident "fresh2" Nothing, Ref TBool))
                                            (Lit TBool (LBool True))
                                    , Wait [Dynamic (Ident "fresh2" Nothing, Ref TBool)]
                                    ]
                                ]
                  }
                )
              ]
  , peripherals = []
  }

spec :: H.Spec
spec = T.propSyntacticEquality "FlipFlopLoop" (toProgram p1) p
