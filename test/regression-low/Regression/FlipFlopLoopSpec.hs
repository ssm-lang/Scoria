-- | A test that alternately assigns True and False to a bool reference at 10Hz.
--
-- Functionally equivalent to blink examples, useful for hardware testing.
module Regression.FlipFlopLoopSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

import SSM.Compile

p :: Program
p = Program
  { entry = Ident "fun0" Nothing
  , args  = []
  , funs  = fromList
              [ ( Ident "fun0" Nothing
                , Procedure
                  { name = Ident "fun0" Nothing
                  , arguments = []
                  , body = [ NewRef (Ident "ref2" Nothing) TBool (Lit TBool (LBool True))
                           , Fork [(Ident "fun1" Nothing, [Right (Dynamic (Ident "ref2" Nothing, TBool))])]
                           ]
                  }
                )
              , ( Ident "fun1" Nothing
                , Procedure
                  { name      = Ident "fun1" Nothing
                  , arguments = [(Ident "ref2" Nothing, Ref TBool)]
                  , body      = [ While
                                    (Lit TBool (LBool True))
                                    [ After (Lit TUInt64 (LUInt64 2))
                                            (Dynamic (Ident "ref2" Nothing, Ref TBool))
                                            (Lit TBool (LBool False))
                                    , Wait [Dynamic (Ident "ref2" Nothing, Ref TBool)]
                                    , After (Lit TUInt64 (LUInt64 2))
                                            (Dynamic (Ident "ref2" Nothing, Ref TBool))
                                            (Lit TBool (LBool True))
                                    , Wait [Dynamic (Ident "ref2" Nothing, Ref TBool)]
                                    ]
                                ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.correctSpec "FlipFlopLoop" p
