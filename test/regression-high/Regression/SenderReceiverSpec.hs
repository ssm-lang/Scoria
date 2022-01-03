-- | Simple demonstration of "unit" event types.
module Regression.SenderReceiverSpec where

import Data.Int
import SSM.Frontend.Compile
import           SSM.Language
import           SSM.Frontend.Box
import           SSM.Frontend.Syntax
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

sender :: Ref () -> SSM ()
sender = box "sender" ["myEvent"] $ \myEvent -> do
    after (mins 1 + secs 1) myEvent event

        
receiver :: Ref () -> SSM ()
receiver = box "receiver" ["myEvent"] $ \myEvent -> do
    wait myEvent

main ::  SSM ()
main = boxNullary "main" $ do
    myEvent <- var event
    fork [ sender myEvent, receiver myEvent ]

program :: Compile backend ()
program = schedule main

spec :: H.Spec
spec = T.correctSpec "SenderReceiver" (toProgram program)
