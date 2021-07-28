module Regression.ManyContsSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = "fun1"
  , args  = [ Right $ Dynamic ("ref2", Ref TUInt64)
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
            (Dynamic ("ref2", Ref TUInt64))
            (Lit TUInt64 (LUInt64 2))
          , Wait [Dynamic ("ref2", Ref TUInt64)]
          , Fork
            [ ( "fun1"
              , [ Right $ Dynamic ("ref2", Ref TUInt64)
                ]
              )
            , ( "fun1"
              , [ Right $ Dynamic ("ref2", Ref TUInt64)
                ]
              )
            , ( "fun1"
              , [ Right $ Dynamic ("ref2", Ref TUInt64)
                ]
              )
            ]
          ]
        }
      )
    ]
  , global_references = []
  }

spec :: H.Spec
spec = T.correctSpec "ManyConts" p
