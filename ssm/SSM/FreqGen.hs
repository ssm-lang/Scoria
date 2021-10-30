{- | Frequency generator example

Pseudocode of intended behavior:

  freqGen (period: &Int) =
    loop
      after period, led0 <- !*led0
      wait led0

  buttonHandler (period: &Int) =
    loop
      wait { sw0 || sw1 }
      if @sw0
        period <- *period * 2
      else // @sw1
        period <- max(*period / 2, 1)

  main =
    let period = new 1s
    par freqGen period
        buttonHandler period

Nits with EDSL noted inline.
-}
{-# LANGUAGE ImplicitParams #-}
module SSM.FreqGen where

import           SSM.Compile
import           SSM.Language
import           SSM.Pretty

import           Data.Word

import           SSM.Frontend.Peripheral.GPIO
import           SSM.Frontend.Peripheral.Identity
import           SSM.Frontend.Peripheral.LED


buttonHandler :: (?sw0::Ref SW, ?sw1::Ref SW) => Ref Word64 -> SSM ()
buttonHandler = box "buttonHandler" ["period"] $ \period -> while true' $ do
  wait (?sw0, ?sw1)
  -- Nit: using parens for each branch is annoying
  ifThenElse (changed ?sw0)
             -- Nit: (<~) is higher precedence than (*)
             (period <~ (deref period * 2))
             -- Nit: (/.) is inconsistent with (*)
             (period <~ max' (deref period /. 2) 1)

freqGen :: (?led0::Ref LED) => Ref Word64 -> SSM ()
freqGen = box "freqGen" ["period"] $ \period -> while true' $ do
  after (nsecs $ deref period) ?led0 (not' $ deref ?led0)
  wait ?led0

-- Nit: implicit params aren't transitive, must be declared by caller
entry :: (?sw0::Ref SW, ?sw1::Ref SW, ?led0::Ref LED) => SSM ()
entry = boxNullary "entry" $ do
  -- Nit: can't construct Ref SSMTime? Marshalling to/from ns is really annoying
  -- and seems error-prone.
  period <- var $ time2ns $ secs 1
  fork [freqGen period, buttonHandler period]

compiler :: Compile ()
compiler = do
  switch0        <- switch 0
  switch1        <- switch 1
  (led, handler) <- onoffLED 0

  let ?led0 = led
      ?sw0  = switch0
      ?sw1  = switch1

  schedule entry
  schedule handler
