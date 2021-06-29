module Regression.ManyContsSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.Ssm.Prop                 as T

p :: Program
p = Program
  { entry = "fun1"
  , args  = [ Right ("ref2", Ref TUInt64)
            ]
  , funs  = fromList
    [ ( "fun1"
      , Procedure
        { name      = "fun1"
        , arguments = [ ("ref2", Ref TUInt64)
                      ]
        , body      =
          [ After
            (Lit TUInt64 (LUInt64 2))
            ("ref2", Ref TUInt64)
            (Lit TUInt64 (LUInt64 2))
          , After
            (Lit TUInt64 (LUInt64 3))
            ("ref2", Ref TInt64)
            (Lit TUInt64 (LUInt64 3))
          , Wait [("ref2", Ref TUInt64)]
          , Fork
            [ ( "fun1"
              , [ Right ("ref2", Ref TUInt64)
                ]
              )
            , ( "fun1"
              , [ Right ("ref2", Ref TUInt64)
                ]
              )
            , ( "fun1"
              , [ Right ("ref2", Ref TUInt64)
                ]
              )
            ]
          ]
        }
      )
    ]
  }

spec :: H.Spec
spec = T.doProgramSpec "ManyConts" p
