module Regression.Test1623630765726Spec where

import           Data.Map                       ( fromList )
import           LowCore
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.Ssm.Prop                 as T

p :: Program
p = Program
  { entry = "fun1"
  , args  = [ Right ("ref2", Ref TUInt64)
            , Right ("ref3", Ref TInt32)
            , Right ("ref4", Ref TUInt64)
            , Right ("ref8", Ref TInt64)
            ]
  , funs  = fromList
    [ ( "fun1"
      , Procedure
        { name      = "fun1"
        , arguments = [ ("ref2", Ref TUInt64)
                      , ("ref3", Ref TInt32)
                      , ("ref4", Ref TUInt64)
                      , ("ref8", Ref TInt64)
                      ]
        , body      =
          [ After
            (Lit TUInt64 (LUInt64 600))
            ("ref4", Ref TUInt64)
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
          , GetRef (Fresh "v0") TInt32 ("ref3", Ref TInt32)
          , GetRef (Fresh "v1") TInt32 ("ref3", Ref TInt32)
          , After
            (Lit TUInt64 (LUInt64 1065))
            ("ref3", Ref TInt32)
            (BOp
              TInt32
              (BOp TInt32 (Var TInt32 "v1") (Var TInt32 "v0") OTimes)
              (BOp TInt32
                   (Lit TInt32 (LInt32 154))
                   (Lit TInt32 (LInt32 172))
                   OPlus
              )
              OPlus
            )
          , Wait [("ref4", Ref TUInt64)]
          , Fork
            [ ( "fun1"
              , [ Right ("ref4", Ref TUInt64)
                , Right ("ref3", Ref TInt32)
                , Right ("ref4", Ref TUInt64)
                , Right ("ref8", Ref TInt64)
                ]
              )
            , ( "fun1"
              , [ Right ("ref2", Ref TUInt64)
                , Right ("ref3", Ref TInt32)
                , Right ("ref2", Ref TUInt64)
                , Right ("ref8", Ref TInt64)
                ]
              )
            , ( "fun1"
              , [ Right ("ref4", Ref TUInt64)
                , Right ("ref3", Ref TInt32)
                , Right ("ref4", Ref TUInt64)
                , Right ("ref8", Ref TInt64)
                ]
              )
            ]
          ]
        }
      )
    ]
  }

spec :: H.Spec
spec = T.doProgramSpec "Test1623630765726Spec" p
