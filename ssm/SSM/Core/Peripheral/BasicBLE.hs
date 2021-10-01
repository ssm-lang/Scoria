{- | This module implements the peripheral that describes basic BLE support. The
part that makes it basic is the fact that this module only supports broadcasting &
scanning, in a very limited manner.

  1. When you scan for messages, you need to specify the MAC-address of the remote board
  you are scanning for messages from.
  2. You can only broadcast/scan a single byta at a time, so the size of the payload
  is very limited.

This module is intended to act as a simple example of what we can do. Our ambition is to
add support for the entire BLE stack.

-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module SSM.Core.Peripheral.BasicBLE where

import           SSM.Core.Ident
import           SSM.Core.Type

import           SSM.Core.Peripheral

import qualified Language.C.Syntax as C

data Backend = C | Python | JS

class IsPeripheral2 (backend :: k) a where
    type Init backend
    init :: a -> [Init backend]

instance IsPeripheral2 C BasicBLE where
    type Init C = C.Stm

-- | Basic BLE data type
data BasicBLE = BasicBLE
    { broadcast        :: (Ident, Type)  -- ^ Name and type of broadcast reference
    , broadcastControl :: (Ident, Type)  -- ^ Name and type of broadcast control reference
    , scan             :: (Ident, Type)  -- ^ Name and type of scan reference
    , scanControl      :: (Ident, Type)  -- ^ Name and type of scan control reference
    }

instance IsPeripheral BasicBLE where
    declaredReferences ble = undefined

    mainInitializers ble = undefined

{- | This function returns a peripheral that enables the BLE stack. The arguments are:

  1. Name and type of the reference used to broadcast messages
  2. Name and type of the reference that is used to control the broadcasting
  functionality
  3. Name and type of the reference used to scan for messages
  4. Name and type of the reference that is used to control the scanning functionality

-}
enableBLE
    :: (Ident, Type)
    -> (Ident, Type)
    -> (Ident, Type)
    -> (Ident, Type)
    -> BasicBLE
enableBLE broadcast broadcastControl scan scanControl = BasicBLE
    { broadcast        = broadcast
    , broadcastControl = broadcastControl
    , scan             = scan
    , scanControl      = scanControl
    }
