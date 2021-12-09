{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
module SSM.Frontend.Peripheral.BasicBLE
  ( BBLE
  , enableBLE
  , enableBroadcast
  , disableBroadcast
  , enableScan
  , disableScan
  , scanref
  , SupportBBLE
  )
  where

import SSM.Core hiding (BasicBLE(..), peripherals, enableBLE)

import SSM.Backend.C.Identifiers
import SSM.Backend.C.Types

import SSM.Frontend.Compile
import SSM.Frontend.Ref
import SSM.Language

import Data.Proxy
import Data.Word
import qualified Data.Map as Map

import Control.Monad.State

import           Language.C.Quote.GCC ( cedecl, cexp, citem, citems )
import qualified Language.C.Syntax as C

data BasicBLE = BasicBLE
  { broadcast_        :: (Ident, Type)
  , broadcastControl_ :: (Ident, Type)
  , scan_             :: (Ident, Type)
  , scanControl_      :: (Ident, Type)
  }
  deriving (Show, Eq)

initBasicBLE :: BasicBLE
initBasicBLE = BasicBLE
  { broadcast_        = (makeIdent "broadcast",        Ref TUInt64)
  , broadcastControl_ = (makeIdent "broadcastControl", Ref TBool)
  , scan_             = (makeIdent "scan",             Ref TUInt64)
  , scanControl_      = (makeIdent "scanControl",      Ref TBool)
  }

instance IsPeripheral C BasicBLE where
    declareReference _ _ id _ _ = error "error --- declareReference BasicBLE called"
    declaredReferences _ bble = map
        (\f -> uncurry makeStaticRef $ f bble)
        [broadcast_, broadcastControl_, scan_, scanControl_]

    globalDeclarations p bble = flip map (declaredReferences p bble) $ \ref -> do
        [cedecl| $ty:(svt_ $ dereference $ refType ref) $id:(refName ref); |]

    staticInitialization p bble =
        let enable   = [cexp| $id:enable_ble_stack() |]
            scanref  = uncurry makeStaticRef (scan_ bble)
            scaninit =  [cexp| $id:initialize_static_input_ble_scan_device(&$id:(refName scanref).sv) |]
        in [citems| $exp:enable; $exp:scaninit; |]

class BLEHandlers backend where
    broadcastHandler        :: proxy backend -> BasicBLE -> Handler backend
    broadcastControlHandler :: proxy backend -> BasicBLE -> Handler backend
    scanControlHandler      :: proxy backend -> BasicBLE -> Handler backend

instance BLEHandlers C where
    broadcastHandler _ bble = Handler
        (\k cs ->
            let (prio,dep) = pdep k cs priority_at_root depth_at_root
                proto      = initialize_static_output_ble_broadcast_device
                refname    = identName $ fst $ broadcast_ bble
            in [[citem| $id:proto(&$id:(refname).sv); |]])
        (concat [ "bind_static_ble_broadcast_handler_device("
                , identName $ fst $ broadcast_ bble
                , ")"])

    broadcastControlHandler _ bble = Handler
        (\k cs ->
            let (prio,dep) = pdep k cs priority_at_root depth_at_root
                proto      = initialize_static_output_ble_broadcast_control_device
                refname    = identName $ fst $ broadcastControl_ bble
            in [[citem| $id:proto(&$id:(refname).sv); |]])
        (concat [ "bind_static_ble_broadcast_control_handler_device("
                , identName $ fst $ broadcastControl_ bble
                , ")"])

    scanControlHandler _ bble = Handler
        (\k cs ->
            let (prio,dep) = pdep k cs priority_at_root depth_at_root
                proto      = initialize_static_output_ble_scan_control_device
                refname    = identName $ fst $ scanControl_ bble
            in [[citem| $id:proto(&$id:(refname).sv); |]])
        (concat [ "bind_static_ble_scan_control_handler_device("
                , identName $ fst $ scanControl_ bble
                , ")"])

-- frontend api of BBLE

data BBLE = BBLE
  { broadcast        :: Ref Word64
  , broadcastControl :: Ref Bool
  , scan             :: Ref Word64
  , scanControl      :: Ref Bool
  }

createBBLE :: BasicBLE -> BBLE
createBBLE bble = BBLE
  { broadcast        = Ptr $ uncurry makeStaticRef $ broadcast_ bble
  , broadcastControl = Ptr $ uncurry makeStaticRef $ broadcastControl_ bble
  , scan             = Ptr $ uncurry makeStaticRef $ scan_ bble
  , scanControl      = Ptr $ uncurry makeStaticRef $ scanControl_ bble
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

bblekey :: String
bblekey = "bblekey"

enableBLE :: forall backend . (IsPeripheral backend BasicBLE, BLEHandlers backend) => Compile backend (BBLE, OutputHandler backend, OutputHandler backend, OutputHandler backend)
enableBLE = do
    modify $ \st -> st {
        peripherals = Map.insert bblekey (Peripheral initBasicBLE) (peripherals st) }

    let ble               = initBasicBLE
        broadcastH        = broadcastHandler (Proxy @backend) ble
        broadcastControlH = broadcastControlHandler (Proxy @backend) ble
        scanControlH      = scanControlHandler (Proxy @backend) ble 
        bble              = createBBLE initBasicBLE

    return (bble, broadcastH, broadcastControlH, scanControlH)

type SupportBBLE backend = ( IsPeripheral backend BasicBLE
                           , BLEHandlers backend
                           )
