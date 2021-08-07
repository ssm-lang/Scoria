-- | Simple demonstration of "unit" event types.
module Regression.SenderReceiverSpec where

import Data.Int
import           SSM.Frontend.Language
import           SSM.Frontend.Box
import           SSM.Frontend.Syntax
import           SSM.Core.Syntax(SSMType(..))
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

sender :: Ref () -> SSM ()
sender = box "sender" ["myEvent"] $ \myEvent -> do
    after (nsecs 2) myEvent event'

        
receiver :: Ref () -> SSM ()
receiver = box "receiver" ["myEvent"] $ \myEvent -> do
    wait [myEvent]

main ::  SSM ()
main = boxNullary "main" $ do
    myEvent <- var event'
    fork [ sender myEvent, receiver myEvent ]

spec :: H.Spec
spec = T.correctSpec "SenderReceiver" main
