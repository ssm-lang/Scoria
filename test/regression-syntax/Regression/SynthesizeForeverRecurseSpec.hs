{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.SynthesizeForeverRecurseSpec where

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
    x <- var (0 :: Exp Int64)
    y <- var (1 :: Exp Int64)
    fork [ do x <~ deref x + deref y
              fork [fun1]
         ]

fun1 :: SSM ()
fun1 = routine $ fork [fun1]

p = Program
  {
      initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
    , funs = fromList [( Ident "generatedfresh0" Nothing
                       , Procedure { name = Ident "generatedfresh0" Nothing
                                   , arguments = [(Ident "fresh0" Nothing, Ref TInt64), (Ident "fresh1" Nothing, Ref TInt64)]
                                   , body = [ SetRef (Dynamic (Ident "fresh0" Nothing, Ref TInt64)) (BOp TInt64 (UOpR TInt64 (Dynamic (Ident "fresh0" Nothing, Ref TInt64)) Deref) (UOpR TInt64 (Dynamic (Ident "fresh1" Nothing, Ref TInt64)) Deref) OPlus)
                                            , Fork [(Ident "fun1" Nothing, [])]
                                            ]})
                       ,( Ident "fun0" Nothing
                        , Procedure (Ident "fun0" Nothing) []
                            [ NewRef (Ident "fresh0" Nothing) TInt64 (Lit TInt64 (LInt64 0))
                            , NewRef (Ident "fresh1" Nothing) TInt64 (Lit TInt64 (LInt64 1))
                            , Fork [(Ident "generatedfresh0" Nothing,[Right $ Dynamic (Ident "fresh0" Nothing, Ref TInt64), Right $ Dynamic (Ident "fresh1" Nothing, Ref TInt64)])]]
                        )
                       , ( Ident "fun1" Nothing
                         , Procedure (Ident "fun1" Nothing) []
                             [Fork [(Ident "fun1" Nothing, [])]])
                       ]
    , peripherals = []}


spec :: H.Spec
spec = T.propSyntacticEquality "SynthesizeForeverRecurse" fun0 p
