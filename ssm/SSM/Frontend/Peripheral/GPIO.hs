{-| This module exposes the GPIO peripheral. The GPIO peripheral enables a developer
to write programs that use GPIO pins either as high/low output or high/low input. -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module SSM.Frontend.Peripheral.GPIO
  ( -- * SUpporting GPIO
    SupportGPIO
    -- * Output GPIO
  , GPIO
  , output
    -- * Input GPIO
  , input
  , Switch
    -- * Controlling GPIO
  , high
  , low
  )
  where

import           SSM.Core             ( IsPeripheral(..)
                                      , Peripheral(..)
                                      , makeIdent
                                      )
import           SSM.Core.Peripheral.GPIO

import           SSM.Frontend.Ref     ( Ref(..) )
import           SSM.Frontend.Compile
import           SSM.Frontend.Syntax
import           SSM.Frontend.Language

import           Data.Proxy           ( Proxy(Proxy) )
import           Data.Word            ( Word8 )
import qualified Data.Map as Map

import           Control.Monad.State ( MonadState(put, get) )

---------- GPIO Output ----------

gpioutputkey :: String
gpioutputkey = "gpioutput"

-- | GPIO output pins have a binary state
type GPIO = Bool

{- | Populates the GPIO pripheral with a new reference.

Parameters:

  1. @Word8@ that identifies the GPIO pin on the board
  2. The name of the reference

Returns: The @Ref LED@ that represents the newly created reference. -}
insertGPIOutput :: forall backend
           .  IsPeripheral backend GPIOOutput
           => Word8 -> Ident -> Compile backend Reference
insertGPIOutput i id = do
    st <- get

    -- fetch the GPIO peripheral and populate it with the new reference
    let maybegpio = Map.lookup gpioutputkey (peripherals st)
        emptyperi = Peripheral @backend emptyGPIOutput
        m         = maybe emptyperi (\x -> x) maybegpio
        m'        = declareReference (Proxy @backend) typ id i m

    -- modify the @CompileSt@ to contain the updated GPIO peripheral
    put $ st { peripherals = Map.insert gpioutputkey m' (peripherals st)}

    -- create the reference and return it
    return $ makeStaticRef id typ
  where
      -- | GPIO pins have a binary state, so treating them like @Bool@s seems reasonable
      typ :: Type
      typ = mkReference $ typeOf $ Proxy @Bool

{- | Ask the GPIO peripheral for an output pin that can take the value high or low.
The pin is identified by the @Word8@ parameter.

The output is

  1. A reference that controls the pin
  2. A handler than when scheduled will make sure that the IO is actualized

 -}
output :: forall backend .
       (IsPeripheral backend GPIOOutput, GPIOHandler backend)
       => Word8 -> Compile backend (Ref GPIO, OutputHandler backend)
output i = do
    n <- fresh
    let id = makeIdent ("output" <> show n)

    ref <- insertGPIOutput i id

    let handler = make_handler (Proxy @backend) ref i

    return (Ptr ref, handler)

----------- GPIO Input ----------

-- | GPIO input pins have a binary state
type Switch = Bool

gpinputokey :: String
gpinputokey = "gpinputo"

insertGPInputO :: forall backend
           .  IsPeripheral backend GPIOInput
           => Word8 -> Ident -> Compile backend Reference
insertGPInputO i id = do
    st <- get

    -- fetch the GPIO peripheral and populate it with the new reference
    let maybegpio = Map.lookup gpinputokey (peripherals st)
        emptyperi = Peripheral @backend emptyGPInputO
        m         = maybe emptyperi (\x -> x) maybegpio
        m'        = declareReference (Proxy @backend) typ id i m

    -- modify the @CompileSt@ to contain the updated GPIO peripheral
    put $ st { peripherals = Map.insert gpinputokey m' (peripherals st)}

    -- create the reference and return it
    return $ makeStaticRef id typ
  where
      -- | GPIO pins have a binary state, so treating them like @Bool@s seems reasonable
      typ :: Type
      typ = mkReference $ typeOf $ Proxy @Bool

{- | Ask the GPIO peripheral for an input pin that can take the value high or low.
The pin is identified by the @Word8@ parameter.

The output is a reference that is written to by the GPIO driver when an input is received
 -}
input :: forall backend .
      (IsPeripheral backend GPIOInput, GPIOHandler backend)
      => Word8 -> Compile backend (Ref Switch)
input i = do
    n <- fresh
    let id = makeIdent ("input" <> show n)

    ref <- insertGPInputO i id

    return $ Ptr ref

-- | pin state high
high :: Exp Bool
high = true

-- | pin state low
low :: Exp Bool
low = false

{- | A backend that satisfies the @SupportGPIO@ constraint fully supports both input and
output GPIO pins. -}
type SupportGPIO backend = ( IsPeripheral backend GPIOOutput
                           , IsPeripheral backend GPIOInput
                           , GPIOHandler backend
                           )
