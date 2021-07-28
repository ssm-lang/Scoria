module Regression.MultOverflowIndirectSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = "fun1"
  , args  = []
  , funs  = fromList
              [ ( "fun1"
                , Procedure
                  { name      = "fun1"
                  , arguments = []
                  , body      =
                    [ NewRef (Fresh "v0")
                             TInt32
                             (Lit TInt32 (LInt32 999999))
                    , GetRef (Fresh "v1") TInt32 (Dynamic ("v0", Ref TInt32))
                    , If
                      (BOp
                        TBool
                        (Lit TInt32 (LInt32 0))
                        (BOp TInt32 (Var TInt32 "v1") (Var TInt32 "v1") OTimes)
                        OLT
                      )
                      [ After (Lit TUInt64 (LUInt64 2))
                              (Dynamic ("v0", Ref TInt32))
                              (Lit TInt32 (LInt32 0))
                      ]
                      []
                    , NewRef (Fresh "v3") TInt32 (Lit TInt32 (LInt32 0))
                    , Wait [Dynamic ("v3", Ref TInt32)]
                    ]
                  }
                )
              ]
  , global_references = []
  }

spec :: H.Spec
spec = T.semanticIncorrectSpec "MultOverflowIndirectSpec" p
