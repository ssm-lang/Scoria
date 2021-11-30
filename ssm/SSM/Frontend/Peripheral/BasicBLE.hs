{-# LANGUAGE ImplicitParams #-}
module SSM.Frontend.Peripheral.BasicBLE
    ( BBLE
    , enableBasicBLE
    , enableBroadcast
    , disableBroadcast
    , enableScan
    , disableScan
    , scanref
    ) where

import           SSM.Core.Ident
import           SSM.Core.Peripheral
import           SSM.Core.Peripheral.BasicBLE
                                         hiding ( broadcast
                                                , broadcastControl
                                                , scan
                                                , scanControl
                                                )
import           SSM.Core.Reference      hiding ( Ref )
import           SSM.Core.Type

import           SSM.Frontend.Compile
import           SSM.Frontend.Language
import           SSM.Frontend.Ref
import           SSM.Frontend.Syntax

import           Data.Word

import           Control.Monad.State

{- | Enable the basic BLE device. The returned components are

  1. An object that can be used together with `enableScan`, `disableScan`,
  `enableBroadcast`, `disableBroadcast` and `scanref` to interact with the underlying
  BLE device.
  2. A hander that has to be scheduled for the `enableBroadcast` & `disableBroadcast`
  calls to function.
  3. A handler that must be scheduled for the `enableScan` and `disableScan` calls to
  function. The handler can only be acquired by applying this component to a string that
  describes the remote devices MAC address as a string of hex octets, separated by colon.
  E.g "AB:CD:EF:01:23:54"
-}
enableBasicBLE :: Compile (BBLE, SSM (), SSM ())
enableBasicBLE = do
    let basicble = enableBLE broadcast broadcastControl scan scanControl
    modify $ \s -> s { basicblePeripheral = Just basicble }

    let scanref             = makeStaticRef' scan
        broadcastref        = makeStaticRef' broadcast
        scanControlref      = makeStaticRef' scanControl
        broadcastControlref = makeStaticRef' broadcastControl
        bble = BBLE { scan             = Ptr $ scanref
                    , broadcast        = Ptr $ broadcastref
                    , scanControl      = Ptr $ scanControlref
                    , broadcastControl = Ptr $ broadcastControlref
                    }
        broadcastHandler = do
            emit $ Handler $ Output (BLE Broadcast) broadcastref
            emit $ Handler $ Output (BLE BroadcastControl) broadcastControlref
        scanControlHandler =
            emit $ Handler $ Output (BLE ScanControl) scanControlref

    return (bble, broadcastHandler, scanControlHandler)
  where
    scan :: (Ident, Type)
    scan = (Ident "scan" Nothing, Ref TUInt64)

    broadcast :: (Ident, Type)
    broadcast = (Ident "broadcast" Nothing, Ref TUInt64)

    scanControl :: (Ident, Type)
    scanControl = (Ident "scanControl" Nothing, Ref TBool)

    broadcastControl :: (Ident, Type)
    broadcastControl = (Ident "broadcastControl" Nothing, Ref TBool)

    makeStaticRef' :: (Ident, Type) -> Reference
    makeStaticRef' = uncurry makeStaticRef

data BBLE = BBLE
    { scan             :: Ref Word64
    , broadcast        :: Ref Word64
    , scanControl      :: Ref Bool
    , broadcastControl :: Ref Bool
    }

-- | Enable the BLE scanning device
enableScan :: (?ble :: BBLE) => SSM ()
enableScan = toggleControl (scanControl ?ble) true

-- | Disable the BLE scanning device
disableScan :: (?ble :: BBLE) => SSM ()
disableScan = toggleControl (scanControl ?ble) false

-- | Enable the BLE broadcasting device, broadcasting the specified message
enableBroadcast :: (?ble :: BBLE) => Exp Word64 -> SSM ()
enableBroadcast value = do
    after (nsecs 1) (broadcast ?ble) value
    wait (broadcast ?ble)
    toggleControl (broadcastControl ?ble) true

-- | Disable the BLE broadcasting device
disableBroadcast :: (?ble :: BBLE) => SSM ()
disableBroadcast = toggleControl (broadcastControl ?ble) false

-- | The reference that can be used to wait for scanned messages
scanref :: (?ble :: BBLE) => Ref Word64
scanref = scan ?ble

toggleControl :: Ref Bool -> Exp Bool -> SSM ()
toggleControl ctrl command = do
    after (nsecs 1) ctrl command
    wait ctrl
