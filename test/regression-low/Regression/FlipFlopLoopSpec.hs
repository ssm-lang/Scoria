module Regression.FlipFlopLoopSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = "fun0"
  , args  = []
  , funs  = fromList
              [ ( "fun0"
                , Procedure
                  { name      = "fun0"
                  , arguments = []
                  , body      = [ NewRef (Fresh "ref2")
                                         TBool
                                         (Lit TBool (LBool True))
                                , Fork [("fun1", [Right ("ref2", TBool)])]
                                ]
                  }
                )
              , ( "fun1"
                , Procedure
                  { name      = "fun1"
                  , arguments = [("ref2", Ref TBool)]
                  , body      = [ While
                                    (Lit TBool (LBool True))
                                    [ After (Lit TUInt64 (LUInt64 2))
                                            ("ref2", Ref TBool)
                                            (Lit TBool (LBool False))
                                    , Wait [("ref2", Ref TBool)]
                                    , After (Lit TUInt64 (LUInt64 2))
                                            ("ref2", Ref TBool)
                                            (Lit TBool (LBool False))
                                    , Wait [("ref2", Ref TBool)]
                                    ]
                                ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.correctSpec "FlipFlopLoop" p
