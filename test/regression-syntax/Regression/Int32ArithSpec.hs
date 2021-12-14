{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.Int32ArithSpec where

import           Prelude hiding (sum)

import SSM.Language
import SSM.Frontend.Peripheral.Identity
import SSM.Core
import Data.Map as Map
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T
import Data.Int

fun0 :: SSM ()
fun0 = routine $ do
    fork [ fun3 $ (1-1) - (11509+1)]

fun3 :: Exp Int64 -> SSM ()
fun3 var3 = routine $ return ()

p :: Program backend
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
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
  , peripherals = []
  }

spec :: H.Spec
spec = T.propSyntacticEquality "Int32Arith" fun0 p
