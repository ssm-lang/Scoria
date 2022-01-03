{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module Regression.GlobalEventSpec where

import           Prelude hiding (sum)

import SSM.Language
import SSM.Frontend.Peripheral.Identity
import SSM.Frontend.Compile hiding ( initialQueueContent, peripherals )
import SSM.Core
import Data.Map as Map
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T
import Data.Word

program :: Backend backend => Compile backend ()
program = do
    glob0 <- global @Word8
    let ?glob0 = glob0
    schedule fun0

fun0 :: (?glob0 :: Ref Word8) => SSM ()
fun0 = routine $ return ()

p :: Backend backend => Program backend
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
  , peripherals = [Peripheral $ Globals (fromList [(Ident "glob0" Nothing, Ref TUInt8)])]
  }

spec :: H.Spec
spec = T.propSyntacticEquality "GlobalEvent" (toProgram program) p
