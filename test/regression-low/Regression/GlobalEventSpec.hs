-- | Global variable with an empty procedure.
--
-- Bug encountered: CodeGen threw an error when trying to find the base data
-- type of a Ref TUInt8.
-- Cause: The types of global variable are stored as Refs, but the code did not
-- strip the Ref away.
-- Fix: Fix the code handling global references.
-- Fixed: 89c3a27697f3189848b19c2a7657d2a6f4af659d
module Regression.GlobalEventSpec where

import           Data.Map                       ( fromList )
import           SSM.Core
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

spec :: H.Spec
spec = T.correctSpec "GlobalEventSpec" p

p :: Program
p = Program
  { initialQueueContent = [SSMProcedure (Ident "fun0" Nothing) []]
  , funs             = fromList
                         [ ( Ident { identName = "fun0", identSrcInfo = Nothing }
                           , Procedure
                             { name = Ident { identName = "fun0", identSrcInfo = Nothing }
                             , arguments = []
                             , body = []
                             }
                           )
                         ]
  , peripherals = [Peripheral $ IdentityPeripheral (fromList [(Ident "glob0" Nothing, Ref TUInt8)])]
  }
