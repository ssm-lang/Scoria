-- | Whether an instantaneous assignment overwrites a previously scheduled
-- delayed assignment.
--
-- fun1 (r1: &bool, r3: &i32) =
--   after 2, r1 <- True
--   after 1, r3 <- 3
--   r3 <- 4
--   wait r1
--   wait r3
module Regression.LaterAssignOverwriteSpec where

import           Data.Map                       ( fromList )
import           SSM.Core
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program backend
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs  = fromList
              [ ( Ident "fun0" Nothing
                , Procedure
                  { name      = Ident "fun0" Nothing
                  , arguments = []
                  , body      =
                    [ NewRef (Ident "ref1" Nothing) TBool  (Lit TBool (LBool True))
                    , NewRef (Ident "ref3" Nothing) TInt32 (Lit TInt32 (LInt32 0))
                    , Fork
                      [(Ident "fun1" Nothing, [Right (Dynamic (Ident "ref1" Nothing, Ref TBool)), Right (Dynamic (Ident "ref3" Nothing, Ref TInt32))])]
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
spec = T.correctSpec "LaterAssignOverwriteSpec" p
