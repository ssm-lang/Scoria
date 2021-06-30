module Regression.MultOverflowIndirectSpec where

import           Data.Map                       ( fromList )
import           LowCore
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.Ssm.Prop                 as T

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
                             (Ref TInt32)
                             (Lit TInt32 (LInt32 999999))
                    , GetRef (Fresh "v1") TInt32 ("v0", Ref TInt32)
                    , If
                      (BOp
                        TBool
                        (Lit TInt32 (LInt32 0))
                        (BOp TInt32 (Var TInt32 "v1") (Var TInt32 "v1") OTimes)
                        OLT
                      )
                      [ After (Lit TUInt64 (LUInt64 2))
                              ("v0", Ref TInt32)
                              (Lit TInt32 (LInt32 0))
                      ]
                      []
                    , NewRef (Fresh "v3") (Ref TInt32) (Lit TInt32 (LInt32 0))
                    , Wait [("v3", Ref TInt32)]
                    ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.semanticIncorrectSpec "MultOverflowIndirectSpec" p