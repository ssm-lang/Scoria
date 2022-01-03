{- | This module implements core support for describing the BasicBLE peripheral,
Basically BLE. It supports very limited broadcasting and scanning (of 64 bits). -}
{-# LANGUAGE TypeApplications #-}
module SSM.Core.Peripheral.BasicBLE
  ( BasicBLE
  , broadcast_
  , broadcastControl_
  , scan_
  , scanControl_
  , initBasicBLE
  , declareReferenceBasicBLE
  , declaredReferencesBasicBLE
  , BLEHandlers(..)
  ) where

import SSM.Core.Type
import SSM.Core.Ident
import SSM.Core.Reference
import SSM.Core.Program

import Data.Word
import Data.Proxy

{- | Internal representation of BasicBLE. It is just a collection of references to
control different parts of the BLE API. -}
data BasicBLE = BasicBLE
  { broadcast_        :: (Ident, Type)  -- ^ This ref controls broadcast payload
  , broadcastControl_ :: (Ident, Type)  -- ^ This ref controls broadcast status (on/off)
  , scan_             :: (Ident, Type)  -- ^ This ref controls scanned messages
  , scanControl_      :: (Ident, Type)  -- ^ This ref controls scan status (on/off)
  }
  deriving (Show, Eq)

-- | Create @BasicBLE@ default value
initBasicBLE :: BasicBLE
initBasicBLE = BasicBLE
  { broadcast_        = (makeIdent "broadcast",        mkReference $ typeOf $ Proxy @Word64)
  , broadcastControl_ = (makeIdent "broadcastControl", mkReference $ typeOf $ Proxy @Bool)
  , scan_             = (makeIdent "scan",             mkReference $ typeOf $ Proxy @Word64)
  , scanControl_      = (makeIdent "scanControl",      mkReference $ typeOf $ Proxy @Bool)
  }

-- | Populate the BLE object with a reference
declareReferenceBasicBLE :: proxy backend -> Type -> Ident -> Word8 -> BasicBLE -> BasicBLE
declareReferenceBasicBLE _ _ _ _ _ = error "error --- declareReference BasicBLE called"

-- | Retrieve the declared references from the BLE object
declaredReferencesBasicBLE :: proxy backend -> BasicBLE -> [Reference]
declaredReferencesBasicBLE _ bble =
  map (\f -> uncurry makeStaticRef $ f bble)
    [broadcast_, broadcastControl_, scan_, scanControl_]

-- | This class abstracts away the action of creating handlers for a specific backend
class BLEHandlers backend where
    broadcastHandler        :: proxy backend -> BasicBLE -> Handler backend
    broadcastControlHandler :: proxy backend -> BasicBLE -> Handler backend
    scanControlHandler      :: proxy backend -> BasicBLE -> Handler backend
