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
  , funs  = fromList
              [ ( Ident "fun0" Nothing
                , Procedure
                  { name      = Ident "fun0" Nothing
                  , arguments = []
                  , body      =
                    [ CreateRef (Ident "ref1" Nothing) (Ref TBool)
                    , SetRef (Dynamic (Ident "ref1" Nothing, Ref TBool)) (Lit TBool (LBool True))
                    , CreateRef (Ident "ref3" Nothing) (Ref TInt32)
                    , SetRef (Dynamic (Ident "ref3" Nothing, Ref TInt32)) (Lit TInt32 (LInt32 0))
                    , Fork
                      [(Ident "fun1" Nothing, [Right (Dynamic (Ident "ref1" Nothing, Ref TBool)), Right (Dynamic (Ident "ref3" Nothing, Ref TInt32))])]
                    , Yield
                    ]
                  }
                )
              , ( Ident "fun1" Nothing
                , Procedure
                  { name = Ident "fun1" Nothing
                  , arguments = [(Ident "ref1" Nothing, Ref TBool), (Ident "ref3" Nothing, Ref TInt32)]
                  , body = [ After (SSMTime (Lit TUInt64 (LUInt64 2))
                                            SSMNanosecond)
                                   (Dynamic (Ident "ref1" Nothing, Ref TBool))
                                   (Lit TBool (LBool True))
                           , After (SSMTime (Lit TUInt64 (LUInt64 1))
                                            SSMNanosecond)
                                   (Dynamic (Ident "ref3" Nothing, Ref TInt32))
                                   (Lit TInt32 (LInt32 3))
                           , SetRef (Dynamic (Ident "ref3" Nothing, Ref TInt32)) (Lit TInt32 (LInt32 4))
                           , Sensitize (Dynamic (Ident "ref1" Nothing, Ref TBool))
                           , Yield
                           , Desensitize (Dynamic (Ident "ref1" Nothing, Ref TBool))
                           , Sensitize (Dynamic (Ident "ref3" Nothing, Ref TInt32))
                           , Yield
                           , Desensitize (Dynamic (Ident "ref3" Nothing, Ref TInt32))
                           ]
                  }
                )
              ]
  , globalReferences = []
  }

spec :: H.Spec
spec = T.correctSpec "LaterAssignOverwriteSpec" p
