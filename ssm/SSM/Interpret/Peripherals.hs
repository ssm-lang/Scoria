{- | This module gives instances for various typeclasses that are used to interpret
programs that make use of peripherals. -}
{-# LANGUAGE MultiParamTypeClasses #-}
module SSM.Interpret.Peripherals where

import SSM.Core
import SSM.Core.Peripheral.GPIO
import SSM.Core.Peripheral.BasicBLE

instance IsPeripheral Interpret GPIOOutput where
    declareReference            = declareReferenceGPIOutput
    declaredReferences          = declaredReferencesGPIOutput
    globalDeclarations p gpio   = []
    staticInitialization p gpio = []

instance GPIOHandler Interpret where
  make_handler _ _ _ = Handler $ \_ _ -> []

instance IsPeripheral Interpret GPIOInput where
    declareReference            = declareReferenceGPInputO
    declaredReferences          = declaredReferencesGPInputO
    globalDeclarations p gpio   = []
    staticInitialization p gpio = []

instance IsPeripheral Interpret BasicBLE where
    declareReference            = declareReferenceBasicBLE
    declaredReferences          = declaredReferencesBasicBLE
    globalDeclarations p bble   = []
    staticInitialization p bble = []

instance BLEHandlers Interpret where
  broadcastHandler _ _        = Handler $ \_ _ -> []
  broadcastControlHandler _ _ = Handler $ \_ _ -> []
  scanControlHandler _ _      = Handler $ \_ _ -> []
