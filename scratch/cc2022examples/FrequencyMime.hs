{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
module SSM.FrequencyMime where

import Prelude

import SSM.Core.Backend
import SSM.Language

import SSM.Frontend.Peripheral.GPIO
import SSM.Frontend.Peripheral.Identity
import SSM.Frontend.Peripheral.BasicBLE

import Data.Word

import SSM.Compile

-- utility functions

delay :: Exp Time -> SSM ()
delay time = fork [ delayP time ]

delayP :: Exp Time -> SSM ()
delayP time = routine $ do
    wake <- var event
    after time wake event
    wait wake

-- frequency generator

-- listen to BLE every 5 second and mirror the frequency
bleHandler :: (?ble :: BBLE) => Ref Time -> SSM ()
bleHandler period = routine $ do
    enableScan
    while true $ do
      wait scanref
      period <~ nsecs (deref scanref)
      delay (secs 5)

-- generate the frequency 
freqGen :: (?led0::Ref GPIO) => Ref Time -> SSM ()
freqGen period = routine $ while true $ do
  after (deref period) ?led0 (not' $ deref ?led0)
  wait ?led0

entry :: (?ble :: BBLE, ?led0::Ref GPIO) => SSM ()
entry = routine $ do
  period <- var $ secs 1
  fork [freqGen period, bleHandler period]

generator :: (SupportGPIO backend, SupportBBLE backend) => Compile backend ()
generator = do
  (led, handler) <- output 0
  (ble, broadcast, broadcastControl, scanning) <- enableBLE

  let ?led0 = led
      ?ble  = ble

  schedule handler
  schedule entry
  schedule broadcast
  schedule broadcastControl
  schedule scanning

-- frequency counter

{- | Count the frequency on the specified gpio and write the measured period to
the reference @period@. -}
freqCount :: Ref Switch -> Ref Time -> SSM ()
freqCount sw period = routine $ do
    gate  <- var event
    count <- var $ u64 0

    after (secs 1) gate event
    while true $ do
        if changed gate
            then do
                period <~ secs 1 /. nsecs (deref count)
                -- print frequency
                if changed sw
                    then count <~ 1
                    else count <~ 0
                after (secs 1) gate event
                wait gate -- sleep for 1 sec
                after (secs 1) gate event
            else count <~ deref count + 1
        wait (gate, sw)

freqCount2 :: Ref Switch -> Ref Time -> SSM ()
freqCount2 sw period = routine $ while true $ do
    period <~ (msecs 200)
    delay (secs 5)
    period <~ (msecs 500)
    delay (secs 5)

{- | Every 5 seconds, broadcast the period on the @period@ reference over BLE. -}
broadcastCount :: (?ble :: BBLE) => Ref Time -> SSM ()
broadcastCount count = routine $ while true $ do
    enableBroadcast $ time2ns $ deref count
    delay (secs 5) -- wait for 5 seconds and pray to god that the remote device picks it up
    disableBroadcast

-- | Entry-point for the frequency counter
counterEntry :: (?ble :: BBLE, ?sw :: Ref Switch) => SSM ()
counterEntry = routine $ do
    count <- var $ secs 1
    fork [ freqCount2 ?sw count, broadcastCount count ]

counter :: (SupportGPIO backend, SupportBBLE backend) => Compile backend ()
counter = do
    sw <- input 0
    (ble, broadcast, broadcastControl, scanning) <- enableBLE

    let ?sw  = sw
        ?ble = ble
    
    schedule counterEntry
    schedule broadcast
    schedule broadcastControl
    schedule scanning
