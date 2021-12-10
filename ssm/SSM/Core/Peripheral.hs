{- | This module implements functionality related to talking about peripherals.

All peripherals must implement the @IsPeripheral@ typeclass, once for each backend
that supports the peripheral.

To get a common interface to talk about peripherals that abstracts away the type of
the actual peripheral, we use a GADT @Peripheral@. @Peripheral@ only talks about the
backend the peripheral supports, not the type of the peripheral itself. -}
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

-- | @IsPeripheral@ describes everything that a peripheral is and what it can do
class IsPeripheral backend a where
    {- | Declare a peripheral that is declared in the global scope. The peripheral
    might need to identify some IO driver that it needs to be connected to, which
    is what the @Word8@ parameter is for. -}
    declareReference     :: proxy backend -> Type -> Ident -> Word8 -> a -> a

    {- | Fetch a list of all the references that has been declared in the global scope
    by this peripheral. -}
    declaredReferences   :: proxy backend -> a -> [Reference]

    {- | Fetch a list of declarations that needs to be done at the top-level in
    the generated program. This could be variable declarations, type declarations,
    functions etc.
    
    NOTE: This list of definitions don't need to declare the references. This should
    be handled by each respective backend by calling @declaredReferences@ and using
    that alone to generate the reference definitions. -}
    globalDeclarations   :: proxy backend -> a -> [Definition backend]

    {- | Fetch the statements that make up the static initialization of this peripheral.
    These statements must be inserted in the generated setup procedure.
    
    NOTE: This list of initialization statements don't need to initialize the references
    declared by a peripheral. This should be handled by each respective backend by
    calling @declaredReferences@ and using that information to do initialization.
    It should be assumed that this initialization has happened before these
    statements are executed. -}
    staticInitialization :: proxy backend -> a -> [Statement backend]

-- | Dummy instance to prevent the need for wrapping/unwrapping of @Peripherals@
instance IsPeripheral backend (Peripheral backend) where
    declareReference proxy t id i (Peripheral p) =
        Peripheral $ declareReference proxy t id i p
    declaredReferences proxy (Peripheral p)      = declaredReferences proxy p
    globalDeclarations proxy (Peripheral p)      = globalDeclarations proxy p
    staticInitialization proxy (Peripheral p)    = staticInitialization proxy p
