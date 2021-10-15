-- | A test that applies a wrong number of arguments to a procedure
module Regression.ArgsLenSpec where

import           Data.Map                       ( fromList )
import SSM.Core.Syntax
    ( SSMExp(Lit, Var),
      SSMLit(LUInt64, LBool, LInt32),
      SSMTime(SSMTime),
      Stm(After, NewRef, SetLocal, Fork))
import SSM.Core.Program               ( Program )
import SSM.Core.Ident                 ( Ident )
import SSM.Core.Peripheral            ( Peripheral )
import SSM.Core.Reference             ( Reference )
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
                           , SetLocal (Ident "var1" Nothing) TInt32 (Var TInt32 (Ident "var0" Nothing))
                           ]
                  }
                ),
                ( Ident "fun1" Nothing
                , Procedure
                  { name = Ident "fun1" Nothing
                  , arguments = []
                  , body = [ Fork [ (Ident "fun0" Nothing, [Left (Lit TInt32 (LInt32 42))])]]
                  }
                )
              ]
  , globalReferences = []
  }

spec :: H.Spec
spec = T.typeIncorrectSpec "ArgLen" p
