{- | This module implements the data types and functions necessary to specify which GPIO
peripherals a program uses. GPIOs come in three main flavours:

  1. Switches -- input GPIOs that can be read (either HIGH or LOW)
  2. DACs
  3. ADCs

but this module only implements support for switches so far.

The idea is that this will act as the core representation of the GPIO peripherals. When
code is being generated and the interpreter is ran, this is the datatype that will
describe which pins are being used, what type of pin they are and what names they were
assigned. Regardless of how the frontend language lets the programmer interact with GPIO,
this describes how the core syntax interacts with GPIO. -}
module SSM.Core.Peripheral.GPIO
    ( GPIOPeripheral
    , switchpins
    , emptyGPIOPeripheral
    , addSwitchGPIO
    ) where

import qualified Data.Map                      as Map
import           SSM.Core.Ident

-- | Datatype that describes which GPIO pins are used.
data GPIOPeripheral = GPIOPeripheral
    { {- | This map associates a pin number with the name the reference is given in the
      source code. -}
      switchpins' :: Map.Map Int Ident
    }
    deriving (Show, Read, Eq)

{- | Create an initial GPIO Peripheral description. In the initial description, no GPIO
pins are used. -}
emptyGPIOPeripheral :: GPIOPeripheral
emptyGPIOPeripheral = GPIOPeripheral Map.empty

-- | Add a switch to a `GPIOPeripheral` and get the new peripheral back.
addSwitchGPIO :: Int -> Ident -> GPIOPeripheral -> GPIOPeripheral
addSwitchGPIO i id p = case Map.lookup i (switchpins' p) of
    Just _ -> error $ concat
        [ "SSM.Core.Peripheral.GPIO error: attempt to add switch "
        , show i
        , " but that switch has already been initialized"
        ]
    Nothing -> p { switchpins' = Map.insert i id (switchpins' p) }

-- | Get the switch GPIO pins from a `GPIOPeripheral`
switchpins :: GPIOPeripheral -> [(Int, Ident)]
switchpins gp = Map.toList $ switchpins' gp
