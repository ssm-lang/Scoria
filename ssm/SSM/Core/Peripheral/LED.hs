{- | Core representation of LED peripherals. -}
module SSM.Core.Peripheral.LED where

import           SSM.Core.Ident                 ( Ident )
import           SSM.Core.Peripheral            ( Initializer(Normal)
                                                , IsPeripheral(..)
                                                )
import           SSM.Core.Reference             ( makeStaticRef )
import           SSM.Core.Type                  ( Type(TBool)
                                                , mkReference
                                                )

import qualified Data.Map                      as Map
import           Data.Word                      ( Word8 )

-- | LED peripherals
data LEDPeripheral = LEDPeripheral
    { -- | Associate LED IDs with reference identifiers
      onoffLEDs' :: Map.Map Word8 Ident
    }
    deriving (Eq, Show, Read)

-- | `IsPeripheral` instance for `LEDPeripheral`, so that we can compile `LEDPeripheral`s.
instance IsPeripheral LEDPeripheral where
    declaredReferences lp =
        map (flip makeStaticRef (mkReference TBool) . snd) $ onoffLEDs lp

    mainInitializers lp = concatMap initializeSingle $ onoffLEDs lp
      where
        initializeSingle :: (Word8, Ident) -> [Initializer]
        initializeSingle (_, id) =
            let ref = makeStaticRef id $ mkReference TBool in [Normal ref]

-- | Create an initial LED peripheral
emptyLEDPeripheral :: LEDPeripheral
emptyLEDPeripheral = LEDPeripheral Map.empty

-- | Register a new ON-OFF LED
addOnOffLED :: Word8 -> Ident -> LEDPeripheral -> LEDPeripheral
addOnOffLED i id lp = case Map.lookup i (onoffLEDs' lp) of
    Just _ -> error $ concat
        [ "SSM.Core.Peripheral.LED error: attempt to initialize LED "
        , show i
        , "but that LED has already been initialized"
        ]
    Nothing -> lp { onoffLEDs' = Map.insert i id $ onoffLEDs' lp }

-- | Get all ON-OFF LEDs
onoffLEDs :: LEDPeripheral -> [(Word8, Ident)]
onoffLEDs lp = Map.toList $ onoffLEDs' lp
