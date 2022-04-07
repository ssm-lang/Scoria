{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
module SSM.FibGen where

import Prelude

import SSM.Compile

import SSM.Language
import SSM.Frontend.Peripheral.GPIO
import SSM.Frontend.Peripheral.LED

import Data.Int
import Data.Word


program :: Compile ()
program = do
    (led, handler) <- onoffLED 4

    let ?led = led

    schedule handler
    schedule main

main :: (?led :: Ref LED) => SSM ()
main = routine $ fork [initgen 1000]

initgen :: (?led :: Ref LED) => Exp Word64 -> SSM ()
initgen frequency = routine $ do
    let period = 1000000000 /. frequency
    fork [ freqgen (nsecs period) ]

freqgen :: (?led :: Ref LED) => Exp Time -> SSM ()
freqgen period = routine $ do
    while true $ do
      after period ?led on
      wait ?led
      fork [ fib 30 ]
      after period ?led off
      wait ?led
      fork [ fib 30 ]

loop :: Exp Int32 -> Exp Int32 -> Exp Int32 -> Exp Int32 -> SSM ()
loop i a b n = routine $ do
    if i ==. n
        then return ()
        else fork [ loop (i+1) b (a+b) n ]

loop2 :: Exp Int32 -> Exp Int32 -> Exp Int32 -> Exp Int32 -> SSM ()
loop2 i a b n = routine $ do
    ir <- var i
    ia <- var a
    ib <- var b

    tmpA <- var 0
    tmpB <- var 0
    while ( deref ir /=. n) $ do
        tmpA <~ deref ib
        tmpB <~ deref ia + deref ib

        ir <~ deref ir + 1
        ia <~ deref tmpA
        ib <~ deref tmpB
        


fib :: Exp Int32 -> SSM ()
fib n = routine $ fork [ loop2 0 0 1 n ]
