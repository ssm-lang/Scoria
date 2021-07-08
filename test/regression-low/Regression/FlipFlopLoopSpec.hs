module Regression.FlipFlopLoopSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = "fun1"
  , args  = [Right ("ref2", Ref TUInt64)]
  , funs  = fromList
              [ ( "fun1"
                , Procedure
                  { name      = "fun1"
                  , arguments = [("ref2", Ref TUInt64)]
                  , body      = [ While
                                    (Lit TBool (LBool True))
                                    [ After (Lit TUInt64 (LUInt64 2))
                                            ("ref2", Ref TUInt64)
                                            (Lit TUInt64 (LUInt64 0))
                                    , Wait [("ref2", Ref TUInt64)]
                                    , After (Lit TUInt64 (LUInt64 2))
                                            ("ref2", Ref TUInt64)
                                            (Lit TUInt64 (LUInt64 1))
                                    , Wait [("ref2", Ref TUInt64)]
                                    ]
                                ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.correctSpec "FlipFlopLoop" p
