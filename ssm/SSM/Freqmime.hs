{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
--{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
module SSM.Freqmime where

import           SSM.Compile
import           SSM.Language
import           SSM.Pretty
import qualified SSM.Core as C

import           Data.Word

import           SSM.Frontend.Peripheral.GPIO
import           SSM.Frontend.Peripheral.LED
import SSM.Frontend.Peripheral.Identity ( global )

schedule' :: SSM () %1 -> Compile ()
schedule' = undefined

createbutton :: Int -> (Ref SW -> SSM () %1 -> Compile ()) -> Compile ()
createbutton _ g = g undefined undefined

compileprogram :: Compile ()
compileprogram = createbutton 5 $ \ button handler -> schedule' handler

gate_period :: SSMTime
gate_period = secs 1

blink_time :: SSMTime
blink_time = msecs 100

type Frequency = Word64

freq_count :: Ref SW -> Ref SW -> Ref Frequency -> SSM ()
freq_count = box "freq_count" ["gate", "signal", "freq"] $ \gate signal freq -> do
    wake <- var event'
    count <- var 0
    while true' $ do
        ifThen (unchanged gate) $ do
            wait gate
        
        ifThenElse (changed signal)
            (count <~ 1)
            (count <~ 0)
        after gate_period wake event'

        doWhile (do
            wait (signal, wake)
            count <~ (deref count + 1))
            (unchanged wake)

        freq <~ (deref count * time2ns (secs 1 // time2ns gate_period))

freq_mime :: Ref Frequency -> Ref LED -> SSM ()
freq_mime = box "freq_mime" ["freq", "led_ctl"] $ \freq led_ctl -> do
    wake <- var event'
    while true' $ do

        ifThen (deref freq /=. 0) $ do
            led_ctl <~ true'
            after (secs 1 // deref freq) wake event'

        wait (wake, freq)

one_shot :: Ref Frequency -> Ref LED -> SSM ()
one_shot = box "one_shot" ["freq", "led_ctl"] $ \freq led_ctl -> do
    while true' $ do
        wait led_ctl

        -- try to calculate delay for when it should turn off
        -- delay will be stored in @delay@ as nanoseconds
        delay <- var 0
        ifThenElse (deref freq /=. 0)
          (delay <~ (time2ns $ lift2T min' blink_time (secs 1 // deref freq // 2)))
          (delay <~ time2ns blink_time)

        ifThen (deref led_ctl) $ do
            after (nsecs $ deref delay) led_ctl false'

mmain :: (?sw0::Ref SW, ?sw1::Ref SW, ?led_ctl::Ref Bool) => SSM ()
mmain = boxNullary "mmain" $ do
    freq <- var $ u64 0
    fork
        [ freq_count ?sw0 ?sw1 freq
        , freq_mime freq ?led_ctl
        , one_shot freq ?led_ctl
        ]

testprogram :: Compile ()
testprogram = do
    x <- switch 0
    y <- switch 1
    (z, handler) <- onoffLED 0

    let ?sw0 = x
        ?sw1 = y
        ?led_ctl = z

    schedule mmain
    schedule handler






mmmain :: Compile ()
mmmain = do
    (x, handler) <- onoffLED 0

    let ?led = x

    schedule handler
    schedule mmain
  where
      mmain :: (?led :: Ref LED) => SSM ()
      mmain = boxNullary "mmain" $ do
          while true' $ do
              after (secs 1) ?led on
              wait ?led
              after (secs 1) ?led off
              wait ?led

testGlobal :: Compile ()
testGlobal = do
    x <- global @Word8
    y <- global @Word64

    let ?x = x
        ?y = y

    schedule prog
  where
      prog :: (?x :: Ref Word8, ?y :: Ref Word64) => SSM ()
      prog = boxNullary "prog" $ do
          ?x <~ 5
          ?y <~ 10

switchCase :: (C.SSMType a, Eq a) => Exp a -> [(Exp a, SSM ())] -> SSM ()
switchCase scrutinee [] = return ()
switchCase scrutinee ((c,bdy):rest)
  = ifThenElse (scrutinee ==. c) bdy (switchCase scrutinee rest)
