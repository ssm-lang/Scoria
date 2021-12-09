{- | This module implements functionality that allows us to talk about peripherals.
A peripheral, rihgt now, is something that might declare some variables in the global
scope, and there might be some initial initialization required upon program startup.

The purpose of this module is mainly to not let any details about C leak to the
core representation. An alternative would be to have the core representation use a
\"C-compileable\" constraint instead, but then we would tie the core representation to
the fact that there exists a C backend. -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module SSM.Core.Peripheral
    ( Peripheral(..)
    , IsPeripheral(..)
    ) where

import           Data.Word                      ( Word8 )
import           SSM.Core.Reference             ( Reference )
import           SSM.Core.Type                  ( Type )
import           SSM.Core.Ident                 ( Ident )
import           SSM.Core.Backend

-- | Type of peripherals
data Peripheral backend where
    -- | A `Peripheral` holds an object that has an instance of `IsPeripheral`
    Peripheral :: forall backend a . (IsPeripheral backend a, Show a, Eq a) => a -> Peripheral backend

instance Show (Peripheral backend) where
    show (Peripheral p) = show p

instance Eq (Peripheral backend) where
    (==) = undefined

-- {- | Different types of peripherals might require different kinds of initialization.
-- This type is meant to enumerate the different types of initialization. -}
-- data Initializer
--     = Normal Reference  -- ^ Perform regular initialization of the reference
--     {- | The @StaticInput@ initialization tells us that the reference is an input
--     reference, and that it needs to be initialized as the kind of static input described
--     by the `StaticInputVariant` type. -}
--     | StaticInput StaticInputVariant Reference
--     | Independent IndependentInit

-- data IndependentInit = BLEEnable

-- -- | Static input variants.
-- data StaticInputVariant = Switch Word8 -- ^ Switch GPIO
--                         | BLEScan

-- -- | Different variants of handlers that can be scheduled at the beginning of a program
-- data Handler
-- --    = StaticOutputHandler Reference Word8  -- ^ Static output handlers (LED? only?)
--     = Output StaticOutputVariant Reference
--     deriving (Show, Eq)

-- data StaticOutputVariant
--     = LED Word8
--     | BLE BLEHandler
--   deriving (Show, Eq)

-- data BLEHandler
--     = Broadcast
--     | BroadcastControl
--     | ScanControl
--   deriving (Show, Eq)

-- -- | Class of types that are peripherals
-- class IsPeripheral a where
--     declaredReferences :: a -> [Reference]  -- ^ Globally declared references
--     -- | Initialization to perform before program startup
--     mainInitializers :: a -> [Initializer]

class IsPeripheral backend a where
    declareReference     :: proxy backend -> Type -> Ident -> Word8 -> a -> a
    declaredReferences   :: proxy backend -> a -> [Reference]
    globalDeclarations   :: proxy backend -> a -> [Definition backend]
    staticInitialization :: proxy backend -> a -> [Initialization backend]

instance IsPeripheral backend (Peripheral backend) where
    declareReference proxy t id i (Peripheral p) =
        Peripheral $ declareReference proxy t id i p
    declaredReferences proxy (Peripheral p)      = declaredReferences proxy p
    globalDeclarations proxy (Peripheral p)      = globalDeclarations proxy p
    staticInitialization proxy (Peripheral p)    = staticInitialization proxy p
