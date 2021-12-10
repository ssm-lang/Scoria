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
