-- | Perform some arithmetic with Int32 literals. It overflows in Int32, but not
-- in Int64.
--
-- Bug encountered: trace printed signed (negative), compiled program printed
-- unsigned number.
-- Cause: Bug in type-mapping + trace statement wasn't even using the right
-- formatter.
-- Fix: Fix type-mapping, and add signed_ to trace statement.
-- Fixed: 63aa2418fca0ca12a9d3027aecb78589b0eaa4c9
module Regression.Int32ArithSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

spec :: H.Spec
spec = T.correctSpec "Int32Arith" p

p :: Program
p = Program
  { entry            = Ident { identName = "fun0", identSrcInfo = Nothing }
  , funs             = fromList
    [ ( Ident { identName = "fun0", identSrcInfo = Nothing }
      , Procedure
        { name      = Ident { identName = "fun0", identSrcInfo = Nothing }
        , arguments = []
        , body      = [ Fork
                          [ ( Ident { identName = "fun3", identSrcInfo = Nothing }
                            , [ Left
                                  (BOp
                                    TInt64
                                    (BOp TInt64
                                         (Lit TInt64 (LInt64 1))
                                         (Lit TInt64 (LInt64 1))
                                         OMinus
                                    )
                                    (BOp TInt64
                                         (Lit TInt64 (LInt64 11509))
                                         (Lit TInt64 (LInt64 1))
                                         OPlus
                                    )
                                    OMinus
                                  )
                              ]
                            )
                          ]
                      ]
        }
      )
    , ( Ident { identName = "fun3", identSrcInfo = Nothing }
      , Procedure
        { name      = Ident { identName = "fun3", identSrcInfo = Nothing }
        , arguments = [ ( Ident { identName = "var3", identSrcInfo = Nothing }
                        , TInt64
                        )
                      ]
        , body      = []
        }
      )
    ]
  , globalReferences = []
  }
