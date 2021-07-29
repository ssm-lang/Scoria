module Regression.Test1623630765726Spec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = Ident "fun1" Nothing
  , args  = [ Right (Ident "ref2" Nothing, Ref TUInt64)
            , Right (Ident "ref3" Nothing, Ref TInt32)
            , Right (Ident "ref4" Nothing, Ref TUInt64)
            , Right (Ident "ref8" Nothing, Ref TInt64)
            ]
  , funs  = fromList
    [ ( Ident "fun1" Nothing
      , Procedure
        { name      = Ident "fun1" Nothing
        , arguments = [ (Ident "ref2" Nothing, Ref TUInt64)
                      , (Ident "ref3" Nothing, Ref TInt32)
                      , (Ident "ref4" Nothing, Ref TUInt64)
                      , (Ident "ref8" Nothing, Ref TInt64)
                      ]
        , body      =
          [ After
            (Lit TUInt64 (LUInt64 600))
            (Ident "ref4" Nothing, Ref TUInt64)
            (BOp
              TUInt64
              (BOp TUInt64
                   (Lit TUInt64 (LUInt64 17033))
                   (Lit TUInt64 (LUInt64 1))
                   OTimes
              )
              (BOp TUInt64
                   (Lit TUInt64 (LUInt64 1))
                   (Lit TUInt64 (LUInt64 1))
                   OTimes
              )
              OPlus
            )
          , GetRef ((Ident "v0" Nothing)) TInt32 (Ident "ref3" Nothing, Ref TInt32)
          , GetRef ((Ident "v1" Nothing)) TInt32 (Ident "ref3" Nothing, Ref TInt32)
          , After
            (Lit TUInt64 (LUInt64 1065))
            (Ident "ref3" Nothing, Ref TInt32)
            (BOp
              TInt32
              (BOp TInt32 (Var TInt32 (Ident "v1" Nothing)) (Var TInt32 (Ident "v0" Nothing)) OTimes)
              (BOp TInt32
                   (Lit TInt32 (LInt32 154))
                   (Lit TInt32 (LInt32 172))
                   OPlus
              )
              OPlus
            )
          , Wait [(Ident "ref4" Nothing, Ref TUInt64)]
          , Fork
            [ ( Ident "fun1" Nothing
              , [ Right (Ident "ref4" Nothing, Ref TUInt64)
                , Right (Ident "ref3" Nothing, Ref TInt32)
                , Right (Ident "ref4" Nothing, Ref TUInt64)
                , Right (Ident "ref8" Nothing, Ref TInt64)
                ]
              )
            , ( Ident "fun1" Nothing
              , [ Right (Ident "ref2" Nothing, Ref TUInt64)
                , Right (Ident "ref3" Nothing, Ref TInt32)
                , Right (Ident "ref2" Nothing, Ref TUInt64)
                , Right (Ident "ref8" Nothing, Ref TInt64)
                ]
              )
            , ( Ident "fun1" Nothing
              , [ Right (Ident "ref4" Nothing, Ref TUInt64)
                , Right (Ident "ref3" Nothing, Ref TInt32)
                , Right (Ident "ref4" Nothing, Ref TUInt64)
                , Right (Ident "ref8" Nothing, Ref TInt64)
                ]
              )
            ]
          ]
        }
      )
    ]
  }

spec :: H.Spec
spec = T.correctSpec "Test1623630765726Spec" p
