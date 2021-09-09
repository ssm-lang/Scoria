-- | A test that attempts to construct a bool literal from a uint64.
module Regression.MistypedLitSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

import SSM.Compile

p :: Program
p = Program
  { entry = Ident "fun0" Nothing
  , funs  = fromList
              [ ( Ident "fun0" Nothing
                , Procedure
                  { name = Ident "fun0" Nothing
                  , arguments = []
                  , body = [ NewRef (Ident "ref0" Nothing) TBool (Lit TBool (LBool True))
                           , After (SSMTime (Lit TUInt64 (LUInt64 2))
                                                     SSMNanosecond)
                                            (Dynamic (Ident "ref0" Nothing, Ref TBool))
                                            (Lit TBool  (LUInt64 2))
                           ]
                  }
                )
              ]
  , globalReferences = []
  }

spec :: H.Spec
spec = T.typeIncorrectSpec "MistypedLit" p
