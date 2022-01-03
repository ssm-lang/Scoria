{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
module Regression.CancelBothSpec where

import           Prelude hiding (sum)

import SSM.Frontend.Compile hiding ( initialQueueContent, peripherals )
import SSM.Language
import SSM.Core
import Data.Map as Map
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T


fun0 :: SSM ()
fun0 = routine $ do
    v0 <- var false
    after (nsecs 1) v0 true
    v1 <- var $ changed v0
    after (nsecs 3872) v1 false

p1 :: Compile backend ()
p1 = schedule fun0

p :: Program backend
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs  = fromList
    [ ( Ident "fun0" Nothing
      , Procedure
        { name      = Ident "fun0" Nothing
        , arguments = []
        , body      =
          [ NewRef (Ident "var0" Nothing) TBool (Lit TBool (LBool False))
          , After (Lit TUInt64 (LUInt64 1))
                  (Dynamic (Ident "var0" Nothing, Ref TBool))
                  (Lit TBool (LBool True))
          , NewRef (Ident "var1" Nothing)
                   TBool
                   (UOpR TBool (Dynamic (Ident "var0" Nothing, Ref TBool)) Changed)
          , After (Lit TUInt64 (LUInt64 3872))
                  (Dynamic (Ident "var1" Nothing, Ref TBool))
                  (Lit TBool (LBool False))
          ]
        }
      )
    ]
  , peripherals = []}

spec :: H.Spec
spec = T.propSyntacticEquality "CancelBoth" (toProgram p1) p
