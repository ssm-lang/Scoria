{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.ManyContsSpec where

import           Prelude hiding (sum)

import SSM.Language
import SSM.Frontend.Peripheral.Identity
import SSM.Core
import Data.Map as Map
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T
import Data.Word

fun0 :: SSM ()
fun0 = routine $ do
    fresh0 <- var (0 :: Exp Word64)
    fork [fun1 fresh0]

fun1 :: Ref Word64 -> SSM ()
fun1 ref2 = routine $ do
    after (nsecs 2) ref2 2
    wait ref2
    fork [ fun1 ref2
         , fun1 ref2
         , fun1 ref2
         ]

p :: Program
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs  = fromList
    [ ( Ident "fun0" Nothing
      , Procedure
        { name      = Ident "fun0" Nothing
        , arguments = []
        , body      = [ NewRef (Ident "fresh0" Nothing)
                                TUInt64
                                (Lit TUInt64 (LUInt64 0))
                      , Fork [(Ident "fun1" Nothing, [Right $ Dynamic (Ident "fresh0" Nothing, Ref TUInt64)])]
                      ]
        }
      )
    , ( Ident "fun1" Nothing
      , Procedure
        { name      = Ident "fun1" Nothing
        , arguments = [ (Ident "ref2" Nothing, Ref TUInt64)
                      ]
        , body      =
          [ After
            (SSMTime (Lit TUInt64 (LUInt64 2)))
            (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
            (Lit TUInt64 (LUInt64 2))
          , Wait [Dynamic (Ident "ref2" Nothing, Ref TUInt64)]
          , Fork
            [ ( Ident "fun1" Nothing
              , [ Right $ Dynamic (Ident "ref2" Nothing, Ref TUInt64)
                ]
              )
            , ( Ident "fun1" Nothing
              , [ Right $ Dynamic (Ident "ref2" Nothing, Ref TUInt64)
                ]
              )
            , ( Ident "fun1" Nothing
              , [ Right $ Dynamic (Ident "ref2" Nothing, Ref TUInt64)
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
spec = T.propSyntacticEquality "ManyConts" fun0 p
