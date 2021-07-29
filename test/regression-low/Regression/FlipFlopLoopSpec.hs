-- | A test that alternately assigns True and False to a bool reference at 10Hz.
--
-- Functionally equivalent to blink examples, useful for hardware testing.
module Regression.FlipFlopLoopSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = Ident "fun1" Nothing
  , args  = [Right (Ident "ref2" Nothing, Ref TUInt64)]
  , funs  = fromList
              [ ( Ident "fun1" Nothing
                , Procedure
                  { name      = Ident "fun1" Nothing
                  , arguments = [(Ident "ref2" Nothing, Ref TUInt64)]
                  , body      = [ While
                                    (Lit TBool (LBool True))
                                    [ After (Lit TUInt64 (LUInt64 2))
                                            (Ident "ref2" Nothing, Ref TUInt64)
                                            (Lit TUInt64 (LUInt64 0))
                                    , Wait [(Ident "ref2" Nothing, Ref TUInt64)]
                                    , After (Lit TUInt64 (LUInt64 2))
                                            (Ident "ref2" Nothing, Ref TUInt64)
                                            (Lit TUInt64 (LUInt64 1))
                                    , Wait [(Ident "ref2" Nothing, Ref TUInt64)]
                                    ]
                                ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.correctSpec "FlipFlopLoop" p
