{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
module Regression.CancelBothSpec where

import           Prelude hiding (sum)

import SSM.Language
import SSM.Core
import Data.Map as Map
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T


fun0 :: SSM ()
fun0 = routine $ do
    v0 <- var false'
    after (nsecs 1) v0 true'
    v1 <- var $ changed v0
    after (nsecs 3872) v1 false'

p :: Program
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs  = fromList
    [ ( Ident "fun0" Nothing
      , Procedure
        { name      = Ident "fun0" Nothing
        , arguments = []
        , body      =
          [ NewRef (Ident "fresh0" Nothing) TBool (Lit TBool (LBool False))
          , After (SSMTime (Lit TUInt64 (LUInt64 1)))
                  (Dynamic (Ident "fresh0" Nothing, Ref TBool))
                  (Lit TBool (LBool True))
          , NewRef (Ident "fresh1" Nothing)
                   TBool
                   (UOpR TBool (Dynamic (Ident "fresh0" Nothing, Ref TBool)) Changed)
          , After (SSMTime (Lit TUInt64 (LUInt64 3872)))
                  (Dynamic (Ident "fresh1" Nothing, Ref TBool))
                  (Lit TBool (LBool False))
          ]
        }
      )
    ]
  , peripherals = []}

spec :: H.Spec
spec = T.propSyntacticEquality "CancelBoth" fun0 p
