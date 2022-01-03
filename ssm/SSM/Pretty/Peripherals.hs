{- | This module gives instances for various typeclasses that are used in order
to pretty-print programs that make use of peripherals. -}
{-# LANGUAGE MultiParamTypeClasses #-}
module SSM.Pretty.Peripherals where

import SSM.Core
import SSM.Core.Peripheral.GPIO
import SSM.Core.Peripheral.BasicBLE

import qualified Data.Map as Map

instance IsPeripheral PrettyPrint GPIOOutput where
    declareReference = declareReferenceGPIOutput

    declaredReferences = declaredReferencesGPIOutput

    globalDeclarations p gpio = [
        unlines [ "-- GPIO peripheral output handler:"
                , "-- initialize_static_output_device(ref,id) binds the ref to this procedure"
                , "output_handler(ref,id) {"
                , "  while(true) {"
                , "    wait ref"
                , "    -- actualize value of ref to output pin id"
                , "  }"
                , "}"
                ]
      ]

    staticInitialization p gpio = []

instance GPIOHandler PrettyPrint where
  make_handler _ r i = Handler $ \_ _ ->
    [concat ["initialize_static_output_device(", refName r, ", ", show i, ")"]]

instance IsPeripheral PrettyPrint GPIOInput where
    declareReference = declareReferenceGPInputO

    declaredReferences = declaredReferencesGPInputO

    globalDeclarations p gpio = map init [
        unlines [ "-- GPIO peripheral input handler:"
                , "-- initialize_static_input_device(ref,id) binds the ref to this procedure"
                , "input_handler(ref,id) {"
                , "  while(true) {"
                , "    -- wait for input on pin id"
                , "    -- turn input on pin id to a write to ref"
                , "  }"
                , "}"
                ]
      ]

    staticInitialization _ gpio = flip map (Map.toList (input_ gpio)) $
      \(i,(id,t)) -> concat ["initialize_static_input_device(", identName id, ", ", show i, ")"]

instance IsPeripheral PrettyPrint BasicBLE where
    declareReference = declareReferenceBasicBLE
    declaredReferences = declaredReferencesBasicBLE

    globalDeclarations p bble = map init [
        unlines [ "-- BBLE peripheral broadcast handler:"
                , "-- initialize_static_output_ble_broadcast(ref) binds the ref to this procedure"
                , "broadcast_handler() {"
                , "  while(true) {"
                , concat ["    wait ", identName $ fst $ broadcast_ bble]
                , "    -- reflect value of broadcast ref in BLE broadcast payload"
                , "  }"
                , "}"
                ]
      , unlines [ "-- BBLE peripheral broadcast control handler:"
                , "-- initialize_static_output_ble_broadcast_control(ref) binds the ref to this procedure"
                , "broadcast_control_handler() {"
                , "  while(true) {"
                , concat ["    wait ", identName $ fst $ broadcastControl_ bble]
                , "    -- toggle broadcasting on or off depending on broadcastControl value"
                , "  }"
                , "}"
                ]
      , unlines [ "-- BBLE peripheral scan control handler:"
                , "-- initialize_static_output_ble_scan_control(ref) binds the ref to this procedure"
                , "scan_control_handler() {"
                , "  while(true) {"
                , concat ["    wait ", identName $ fst $ scanControl_ bble]
                , "    -- toggle scanning on or off depending on scanControl value"
                , "  }"
                , "}"
                ]
      , unlines [ "-- BBLE peripheral broadcast handler:"
                , "-- initialize_static_output_ble_scan(ref) binds the ref to this procedure"
                , "scan_handler() {"
                , "  while(true) {"
                , "    -- wait to successfully scan for a received BLE packet"
                , concat ["    -- turn the scanned message into an event on the ", identName $ fst $ scan_ bble, " ref"]
                , "  }"
                , "}"
                ]
      ]

    staticInitialization p bble = [ "enable_ble()"
                                  , concat ["initialize_static_output_ble_scan(", identName $ fst $ scan_ bble, ")"]]

instance BLEHandlers PrettyPrint where
    broadcastHandler _ bble = Handler $ \_ _ ->
      [concat [ "initialize_static_output_ble_broadcast("
              , identName $ fst $ broadcast_ bble, ")"
              ]]

    broadcastControlHandler _ bble = Handler $ \_ _ ->
      [concat [ "initialize_static_output_ble_broadcast_control("
              , identName $ fst $ broadcastControl_ bble, ")"
              ]]

    scanControlHandler _ bble = Handler $ \_ _ ->
      [concat [ "initialize_static_output_ble_scan_control("
              , identName $ fst $ scan_ bble, ")"
              ]]
