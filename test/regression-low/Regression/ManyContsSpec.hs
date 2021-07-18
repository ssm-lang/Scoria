-- | Forkbomb to fill up the continuation queue, but with some delay at each
-- fork.
module Regression.ManyContsSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = "fun0"
  , args  = []
  , funs  = fromList
              [ ( "fun0"
                , Procedure
                  { name      = "fun0"
                  , arguments = []
                  , body      = [ NewRef (Fresh "ref2")
                                         (Ref TUInt64)
                                         (Lit TUInt64 (LUInt64 0))
                                , Fork [("fun1", [Right ("ref2", Ref TUInt64)])]
                                ]
                  }
                )
              , ( "fun1"
                , Procedure
                  { name      = "fun1"
                  , arguments = [("ref2", Ref TUInt64)]
                  , body      = [ After (Lit TUInt64 (LUInt64 2))
                                        ("ref2", Ref TUInt64)
                                        (Lit TUInt64 (LUInt64 2))
                                , Wait [("ref2", Ref TUInt64)]
                                , Fork
                                  [ ("fun1", [Right ("ref2", Ref TUInt64)])
                                  , ("fun1", [Right ("ref2", Ref TUInt64)])
                                  , ("fun1", [Right ("ref2", Ref TUInt64)])
                                  ]
                                ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.correctSpec "ManyConts" p
