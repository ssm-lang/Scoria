module Regression.MultOverflowSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = Ident "fun1" Nothing
  , args  = []
  , funs  = fromList
              [ ( Ident "fun1" Nothing
                , Procedure
                  { name      = Ident "fun1" Nothing
                  , arguments = []
                  , body      = [ NewRef ((Ident "v0" Nothing))
                                         (Ref TInt32)
                                         (Lit TInt32 (LInt32 999999))
                                , If
                                  (BOp
                                    TBool
                                    (Lit TInt32 (LInt32 0))
                                    (BOp TInt32
                                         (Var TInt32 (Ident "v0" Nothing))
                                         (Var TInt32 (Ident "v0" Nothing))
                                         OTimes
                                    )
                                    OLT
                                  )
                                  []
                                  []
                                , NewRef (Ident "v3" Nothing)
                                         (Ref TInt32)
                                         (Lit TInt32 (LInt32 0))
                                , Wait [(Ident "v3" Nothing, Ref TInt32)]
                                ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.semanticIncorrectSpec "MultOverflowSpec" p
