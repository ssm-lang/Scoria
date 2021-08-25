module SSM.Core.Peripheral.LED where

import           SSM.Core.Ident

import qualified Data.Map                      as Map

data LEDPeripheral = LEDPeripheral
    { onoffLEDs' :: Map.Map Int Ident
    }
  deriving (Eq, Show, Read)

emptyLEDPeripheral :: LEDPeripheral
emptyLEDPeripheral = LEDPeripheral Map.empty

addOnOffLED :: Int -> Ident -> LEDPeripheral -> LEDPeripheral
addOnOffLED i id lp = case Map.lookup i (onoffLEDs' lp) of
    Just _ -> error $ concat
        [ "SSM.Core.Peripheral.LED error: attempt to initialize LED "
        , show i
        , "but that LED has already been initialized"
        ]
    Nothing -> lp { onoffLEDs' = Map.insert i id $ onoffLEDs' lp }

onoffLEDs :: LEDPeripheral -> [(Int, Ident)]
onoffLEDs lp = Map.toList $ onoffLEDs' lp
