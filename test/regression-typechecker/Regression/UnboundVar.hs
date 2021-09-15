-- | A test that uses a wrong name for a variable.
module Regression.UnboundVar where

import           Data.Map                       ( fromList )
import SSM.Core.Syntax
    ( Ident(Ident),
      Procedure(Procedure, body, arguments, name),
      Program(..),
      Reference(Dynamic),
      SSMExp(Lit, Var),
      SSMLit(LUInt64, LBool, LInt32),
      SSMTime(SSMTime),
      SSMTimeUnit(SSMNanosecond),
      Stm(After, NewRef, SetLocal),
      Type(Ref, TUInt64, TBool, TInt32) )
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
                           , SetLocal (Ident "var0" Nothing) TInt32 (Lit TInt32 (LInt32 42))
                           , SetLocal (Ident "var1" Nothing) TInt32 (Var TInt32 (Ident "v0" Nothing))
                           ]
                  }
                )
              ]
  , globalReferences = []
  }

spec :: H.Spec
spec = T.typeIncorrectSpec "UnboundVar" p
