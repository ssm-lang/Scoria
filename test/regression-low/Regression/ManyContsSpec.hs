-- | Forkbomb to fill up the continuation queue, but with some delay at each
-- fork.
module Regression.ManyContsSpec where

import           Data.Map                       ( fromList )
import           SSM.Core
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs  = fromList
    [ ( Ident "fun0" Nothing
      , Procedure
        { name      = Ident "fun0" Nothing
        , arguments = []
        , body      = [ NewRef (Ident "ref2" Nothing)
                                TUInt64
                                (Lit TUInt64 (LUInt64 0))
                      , Fork [(Ident "fun1" Nothing, [Right $ Dynamic (Ident "ref2" Nothing, Ref TUInt64)])]
                      ]
        }
      )
    , ( Ident "fun1" Nothing
      , Procedure
        { name      = Ident "fun1" Nothing
        , arguments = [ (Ident "Ref2" Nothing, Ref TUInt64)
                      ]
        , body      =
          [ After
            (Lit TUInt64 (LUInt64 2))
            (Dynamic (Ident "Ref2" Nothing, Ref TUInt64))
            (Lit TUInt64 (LUInt64 2))
          , Wait [Dynamic (Ident "Ref2" Nothing, Ref TUInt64)]
          , Fork
            [ ( Ident "fun1" Nothing
              , [ Right $ Dynamic (Ident "Ref2" Nothing, Ref TUInt64)
                ]
              )
            , ( Ident "fun1" Nothing
              , [ Right $ Dynamic (Ident "Ref2" Nothing, Ref TUInt64)
                ]
              )
            , ( Ident "fun1" Nothing
              , [ Right $ Dynamic (Ident "Ref2" Nothing, Ref TUInt64)
                ]
              )
            ]
          ]
        }
      )
    ]
  , peripherals = []
  }

spec :: H.Spec
spec = T.correctSpec "ManyConts" p
