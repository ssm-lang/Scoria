{-| This module exposes the "basically BLE" peripheral. This peripheral only
implements very limited broadcasting and scanning. When you run @enableBLE@ you
get a @BBLE@ object and three handlers back. The first two handlers control the
broadcast payload and broadcast control, while the last one controls the scan
functionality. The @BBLE@ object acts as a handle that all BBLE functionality
must happen through. -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
module SSM.Frontend.Peripheral.BasicBLE
  ( -- * Accessing the BBLE driver
    BBLE
  , SupportBBLE
  , enableBLE
    -- * Broadcast management
  , enableBroadcast
  , disableBroadcast
    -- * Scan management
  , enableScan
  , disableScan
  , scanref
  )
  where

import SSM.Core hiding (BasicBLE(..), peripherals, enableBLE)
import SSM.Core.Peripheral.BasicBLE

import SSM.Frontend.Compile
import SSM.Frontend.Ref
import SSM.Language

import Data.Proxy
import Data.Word
import qualified Data.Map as Map

import Control.Monad.State

-- | This object can be used to access the BLE driver
data BBLE = BBLE
  { broadcast        :: Ref Word64
  , broadcastControl :: Ref Bool
  , scan             :: Ref Word64
  , scanControl      :: Ref Bool
  }

 -- | Create a @BBLE@ from a @BasicBLE@
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

-- | Key to use when looking up the @BasicBLE@ peripheral from the @Compile@-monad
bblekey :: String
bblekey = "bblekey"

{- | Enable the BBLE driver, and get four things bacl.

  1. @BBLE@ value that can be used to access the BLE driver
  2. Handler that when scheduled make sure that the broadcast payload is
     updated
  3. Handler that when scheduled enables the broadcast control functionality
  4. Handler that when scheduled enables the scan control functionality

-}
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

{- | If a backend satisfies the @SupperBBLE@ constraint, the backend fully supports
the BBLE functionality. -}
type SupportBBLE backend = ( IsPeripheral backend BasicBLE
                           , BLEHandlers backend
                           )
