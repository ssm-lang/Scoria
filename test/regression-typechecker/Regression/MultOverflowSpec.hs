-- | Test for signed integer overflow behavior.
--
-- Note that signed integer overflow is undefined in both C and Haskell, so
-- great care should be taken here.
module Regression.MultOverflowSpec where

import           Data.Map                       ( fromList )
import           SSM.Core
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun1" Nothing) []]
  , funs  = fromList
    [ ( Ident "fun1" Nothing
      , Procedure
        { name      = Ident "fun1" Nothing
        , arguments = []
        , body      =
          [ NewRef ((Ident "v0" Nothing)) TInt32 (Lit TInt32 (LInt32 999999))
          , If
            (BOp TBool
                 (Lit TInt32 (LInt32 0))
                 (BOp TInt32 (UOpR TInt32 (Dynamic (Ident "v0" Nothing, Ref TInt32)) Deref) (UOpR TInt32 (Dynamic (Ident "v0" Nothing, Ref TInt32)) Deref) OTimes)
                 OLT
            )
            [ After (SSMTime (Lit TUInt64 (LUInt64 2)))
                    (Dynamic ((Ident "v0" Nothing), Ref TInt32))
                    (Lit TInt32 (LInt32 0))
            ]
            [ After (SSMTime (Lit TUInt64 (LUInt64 2)))
                    (Dynamic ((Ident "v0" Nothing), Ref TInt32))
                    (Lit TInt32 (LInt32 1))
            ]
          , Wait [Dynamic ((Ident "v0" Nothing), Ref TInt32)]
          ]
        }
      )
    ]
  , peripherals = []}

spec :: H.Spec
spec = T.correctSpec "MultOverflow" p
