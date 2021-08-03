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
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = Ident "fun0" Nothing
  , args  = []
  , funs  = fromList
              [ ( Ident "fun0" Nothing
                , Procedure
                  { name      = Ident "fun0" Nothing
                  , arguments = []
                  , body      =
                    [ NewRef (Ident "ref1" Nothing) (Ref TBool)  (Lit TBool (LBool True))
                    , NewRef (Ident "ref3" Nothing) (Ref TInt32) (Lit TInt32 (LInt32 0))
                    , Fork
                      [(Ident "fun1" Nothing, [Right (Ident "ref1" Nothing, Ref TBool), Right (Ident "ref3" Nothing, Ref TInt32)])]
                    ]
                  }
                )
              , ( Ident "fun1" Nothing
                , Procedure
                  { name = Ident "fun1" Nothing
                  , arguments = [(Ident "ref1" Nothing, Ref TBool), (Ident "ref3" Nothing, Ref TInt32)]
                  , body = [ After (Lit TUInt64 (LUInt64 2))
                                   (Ident "ref1" Nothing, Ref TBool)
                                   (Lit TBool (LBool True))
                           , After (Lit TUInt64 (LUInt64 1))
                                   (Ident "ref3" Nothing, Ref TInt32)
                                   (Lit TInt32 (LInt32 3))
                           , SetRef (Ident "ref3" Nothing, Ref TInt32) (Lit TInt32 (LInt32 4))
                           , Wait [(Ident "ref1" Nothing, Ref TBool)]
                           , Wait [(Ident "ref3" Nothing, Ref TInt32)]
                           ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.correctSpec "LaterAssignOverwriteSpec" p
