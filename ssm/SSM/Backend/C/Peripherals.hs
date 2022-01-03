{- | This module implements the various backend specific typeclasses that are used to
compile peripherals. -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C.Peripherals where

import SSM.Core

import SSM.Core.Peripheral.GPIO
import SSM.Core.Peripheral.BasicBLE

import SSM.Backend.C.Types
import SSM.Backend.C.Identifiers

import qualified Data.Map as Map

import           Language.C.Quote.GCC ( cedecl, cexp, citem, citems )
import qualified Language.C.Syntax as C

instance IsPeripheral C GPIOOutput where
    declareReference            = declareReferenceGPIOutput
    declaredReferences          = declaredReferencesGPIOutput
    globalDeclarations p gpio   = []
    staticInitialization p gpio = []

instance GPIOHandler C where
    make_handler _ r i =
        let sched k cs = let (prio, dep) = pdep k cs priority_at_root depth_at_root
                         in [[citem| $id:initialize_static_output_device(
                                                     $id:top_parent,
                                                     $exp:prio,
                                                     $exp:dep,
                                                     &$id:(refName r).sv,
                                                     $uint:i);|]]
        in Handler sched

instance IsPeripheral C GPIOInput where
    declareReference = declareReferenceGPInputO

    declaredReferences = declaredReferencesGPInputO

    globalDeclarations p gpio = []

    staticInitialization p gpio = flip map (Map.toList (input_ gpio)) $
      \(i,(id,t)) ->
        let 
            bt     = dereference t
            ref    = makeStaticRef id t
            bind   = [cexp| $id:initialize_static_input_device(
                                    (typename ssm_sv_t *) &$id:(refName ref).sv,
                                    $uint:i) |]
        in [citem| $exp:bind; |]

-- | @BasicBLE@ can be compiled to C
instance IsPeripheral C BasicBLE where
    declareReference = declareReferenceBasicBLE
    declaredReferences = declaredReferencesBasicBLE
    globalDeclarations p bble = []

    staticInitialization p bble =
        let enable   = [cexp| $id:enable_ble_stack() |]
            scanref  = uncurry makeStaticRef (scan_ bble)
            scaninit =  [cexp| $id:initialize_static_input_ble_scan_device(&$id:(refName scanref).sv) |]
        in [citems| $exp:enable; $exp:scaninit; |]

-- | The handlers can be compiled to C
instance BLEHandlers C where
    broadcastHandler _ bble = Handler
        (\k cs ->
            let (prio,dep) = pdep k cs priority_at_root depth_at_root
                proto      = initialize_static_output_ble_broadcast_device
                refname    = identName $ fst $ broadcast_ bble
            in [[citem| $id:proto(&$id:(refname).sv); |]])

    broadcastControlHandler _ bble = Handler
        (\k cs ->
            let (prio,dep) = pdep k cs priority_at_root depth_at_root
                proto      = initialize_static_output_ble_broadcast_control_device
                refname    = identName $ fst $ broadcastControl_ bble
            in [[citem| $id:proto(&$id:(refname).sv); |]])

    scanControlHandler _ bble = Handler
        (\k cs ->
            let (prio,dep) = pdep k cs priority_at_root depth_at_root
                proto      = initialize_static_output_ble_scan_control_device
                refname    = identName $ fst $ scanControl_ bble
            in [[citem| $id:proto(&$id:(refname).sv); |]])
