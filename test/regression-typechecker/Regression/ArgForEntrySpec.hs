module Regression.ArgForEntrySpec where
    
import           Data.Map                       ( fromList )
import qualified SSM.Core.Syntax               as S
import qualified SSM.Core.Ident                as I
import qualified SSM.Core.Reference            as R
import qualified SSM.Core.Type                 as T
import qualified SSM.Core.Program              as Prog
import qualified SSM.Core.Peripheral           as Peri
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

import SSM.Compile

p :: Prog.Program
p = Prog.Program
  { Prog.entry = Ident "fun0" Nothing
  , Prog.funs  = fromList
              [ ( Ident "fun0" Nothing
                , Prog.Procedure
                  { Prog.name = Ident "fun0" Nothing
                  , Prog.arguments = [(Ident "arg0" Nothing, TInt32)]
                  , Prog.body = [ NewRef (Ident "ref0" Nothing) TBool (Lit TBool (LBool True))
                           , SetLocal (Ident "var0" Nothing) TInt32 (Lit TInt32 (LInt32 42))
                           , SetLocal (Ident "var1" Nothing) TInt32 (Var TInt32 (Ident "var0" Nothing))
                           ]
                  }
                )
              ]
  , globalReferences = []
  }

spec :: H.Spec
spec = T.typeIncorrectSpec "Entry function has arguments" p
