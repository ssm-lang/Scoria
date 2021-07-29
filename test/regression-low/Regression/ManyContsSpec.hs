module Regression.ManyContsSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = Ident "fun1" Nothing
  , args  = [ Right (Ident "Ref2" Nothing, Ref TUInt64)
            ]
  , funs  = fromList
    [ ( Ident "fun1" Nothing
      , Procedure
        { name      = Ident "fun1" Nothing
        , arguments = [ (Ident "Ref2" Nothing, Ref TUInt64)
                      ]
        , body      =
          [ After
            (Lit TUInt64 (LUInt64 2))
            (Ident "Ref2" Nothing, Ref TUInt64)
            (Lit TUInt64 (LUInt64 2))
          , Wait [(Ident "Ref2" Nothing, Ref TUInt64)]
          , Fork
            [ ( Ident "fun1" Nothing
              , [ Right (Ident "Ref2" Nothing, Ref TUInt64)
                ]
              )
            , ( Ident "fun1" Nothing
              , [ Right (Ident "Ref2" Nothing, Ref TUInt64)
                ]
              )
            , ( Ident "fun1" Nothing
              , [ Right (Ident "Ref2" Nothing, Ref TUInt64)
                ]
              )
            ]
          ]
        }
      )
    ]
  }

spec :: H.Spec
spec = T.correctSpec "ManyConts" p
