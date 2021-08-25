{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module SSM.Freqmime where

import           SSM.Compile
import           SSM.Language
import           SSM.Pretty

import           Data.Word

import           SSM.Frontend.Peripheral.GPIO
import           SSM.Frontend.Peripheral.LED

gate_period :: SSMTime
gate_period = secs 1

blink_time :: SSMTime
blink_time = msecs 100

type Frequency = Word64

freq_count :: Ref SW -> Ref SW -> Ref Frequency -> SSM ()
freq_count =
    box "freq_count" ["gate", "signal", "freq"] $ \gate signal freq -> do
        wake  <- var event'
        count <- var $ u64 0 -- 32
        while true' $ do
            -- wait for gate to begin counting
            wait [gate]

            -- reset counter
            ifThenElse (changed signal) (count <~ u64 1) (count <~ u64 0)
            -- schedule wakeup at end of period
            after gate_period wake event'

            -- until the wake up is signalled, count every time there's a signal
            while (not' $ changed wake) $ do
                wait [signal
                     --, wake
                            ]
                -- if the signal signalled, increment the count
                ifThen (not' (changed wake)) $ do
                    count <~ ((deref count) + 1)

            freq <~ (undefined :: Exp Word64) -- frequency calculation

freq_mime
    :: Ref Frequency
    -> Ref Bool {-LED-}
    -> SSM ()
freq_mime = box "freq_mime" ["freq", "led_ctl"] $ \freq led_ctl -> do
    wake <- var event'
    while true' $ do
        ifThen (changed led_ctl) $ do
            led_ctl <~ true'
            after (secs 1 // deref freq) wake event'

        wait [wake{-, freq -}
                  ]

one_shot
    :: Ref Frequency
    -> Ref Bool {-LED-}
    -> SSM ()
one_shot = box "one_shot" ["freq", "led_ctl"] $ \freq led_ctl -> do
    while true' $ do
        -- wait for led to toggle
        wait [led_ctl]

        -- try to calculate delay for when it should turn off
        delay <- var $ u64 0
        ifThenElse (not' $ deref freq ==. 0)
                   (delay <~ (min' undefined undefined :: Exp Word64))
                   --undefined -- delay <~ MIN(BLINK_TIME, SSM_SECOND / acts->freq->value / 2
                   (delay <~ u64 100)

        -- schedule the turning off of the led
        ifThen (deref led_ctl) $ do
            after undefined led_ctl false'

mmain :: (?sw0::Ref SW, ?sw1::Ref SW, ?led_ctl::Ref Bool) => SSM ()
mmain = boxNullary "mmain" $ do
    freq <- var $ u64 0
    fork
        [ freq_count ?sw0 ?sw1 freq
        , freq_mime freq ?led_ctl
        , one_shot freq ?led_ctl
        ]

testcompile :: Compile (SSM ())
testcompile = do
    x <- switch 0
    y <- switch 1
    let ?sw0     = x
        ?sw1     = y
        ?led_ctl = undefined
    return mmain


testprogram :: Compile (SSM ())
testprogram = do
    x <- switch 0
    y <- switch 1
    z <- onoffLED 0
    let ?sw0 = x
        ?sw1 = y
        ?led_ctl = z
    return testmain
  where
    testmain :: (?sw0::Ref SW, ?sw1::Ref SW) => SSM ()
    testmain = boxNullary "testmain" $ do
        wait [?sw0, ?sw1]
