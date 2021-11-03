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
{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS_GHC -fplugin=SSM.Plugin #-}

module SSM.FreqGen where

{- RebindableSyntax rebinds some things to identifiers that are in scope. It allows
us to e.g rebind (>>=) and return, so without explicitly importing the Prelude (and
and Monad class within it) there will be no such identifiers in scope.
See https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rebindable_syntax.html
for details.
-}
import           Prelude

import           SSM.Compile
import           SSM.Language
import           SSM.Pretty

import           Data.Word

import           SSM.Frontend.Peripheral.GPIO
import           SSM.Frontend.Peripheral.Identity
import           SSM.Frontend.Peripheral.LED


{-# ANN buttonHandler EMBED #-}
buttonHandler :: (?sw0::Ref SW, ?sw1::Ref SW) => Ref Word64 -> SSM ()
buttonHandler period = while true' $ do
  wait (?sw0, ?sw1)
  -- Nit: using parens for each branch is annoying
  if changed ?sw0
     then period <~ deref period * 2
     else period <~ max' (deref period /. 2) 1


{-# ANN freqGen EMBED #-}
freqGen :: (?led0::Ref LED) => Ref Word64 -> SSM ()
freqGen period = while true' $ do
  after (nsecs $ deref period) ?led0 (not' $ deref ?led0)
  wait ?led0

{-# ANN entry EMBED #-}
-- Nit: implicit params aren't transitive, must be declared by caller
entry :: (?sw0::Ref SW, ?sw1::Ref SW, ?led0::Ref LED) => SSM ()
entry = do
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
