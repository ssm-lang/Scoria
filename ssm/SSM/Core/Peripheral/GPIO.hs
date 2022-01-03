{- | This module implements the core support for working with GPIO peripherals.
It exposes two types @GPIOOutput@ and @GPIOInput@ that represent the GPIO pins that
have been requested by a program.-}
module SSM.Core.Peripheral.GPIO
  ( -- * Output
    GPIOOutput
  , output_
  , emptyGPIOutput
  , declareReferenceGPIOutput
  , declaredReferencesGPIOutput
  , GPIOHandler(..)
    -- * Input
  , GPIOInput
  , input_
  , emptyGPInputO
  , declareReferenceGPInputO
  , declaredReferencesGPInputO
  ) where

import SSM.Core.Type
import SSM.Core.Ident
import SSM.Core.Reference
import SSM.Core.Program

import qualified Data.Map as Map

import Data.Word

-- Output

{- | The GPIOOutput datatype represents the GPIO output pins we have requested from the
environment -}
data GPIOOutput = GPIOOutput { output_ :: Map.Map Word8 (Ident, Type)}
  deriving (Show, Eq)

-- | Create an empty GPIOOutput peripheral
emptyGPIOutput :: GPIOOutput
emptyGPIOutput = GPIOOutput { output_ = Map.empty }

{- | Add a reference to the GPIOOutput peripheral. The added reference will be used to
control an output pin, identified by the 4th argument. -}
declareReferenceGPIOutput :: proxy backend -> Type -> Ident -> Word8 -> GPIOOutput -> GPIOOutput
declareReferenceGPIOutput _ t id i gpio = gpio { output_ = Map.insert i (id,t) (output_ gpio) }

-- | Retrieve the declared output references from the GPIOOutput peripheral
declaredReferencesGPIOutput :: proxy backend -> GPIOOutput -> [Reference]
declaredReferencesGPIOutput _ gpio = map (uncurry makeStaticRef) $ Map.elems $ output_ gpio

{- | The @GPIOHandler@ typeclass is parameterised over a backend, and returns a handler
that when scheduled, actually performs the output action. -}
class GPIOHandler backend where
    make_handler :: proxy backend -> Reference -> Word8 -> Handler backend

-- Input

{- | The GPIOInput datatype represents the GPIO input pins we have requested from the
environment -}
data GPIOInput = GPIOInput { input_ :: Map.Map Word8 (Ident, Type) }
  deriving (Show, Eq)

-- | Create an empty GPIOInput peripheral
emptyGPInputO :: GPIOInput
emptyGPInputO = GPIOInput { input_ = Map.empty }

{- | Add a reference to the GPIOInput peripheral. The added reference will be used to
control an input pin, identified by the 4th argument. -}
declareReferenceGPInputO :: proxy backend -> Type -> Ident -> Word8 -> GPIOInput -> GPIOInput
declareReferenceGPInputO _ t id i gpio = gpio { input_ = Map.insert i (id,t) (input_ gpio) }

-- | Retrieve the declared output references from the GPIOOutput peripheral
declaredReferencesGPInputO :: proxy backend -> GPIOInput -> [Reference]
declaredReferencesGPInputO _ gpio = map (uncurry makeStaticRef) $ Map.elems $ input_ gpio
