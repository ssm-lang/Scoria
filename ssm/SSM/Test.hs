{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
module SSM.Test where

import Prelude

import SSM.Language
import SSM.Frontend.Peripheral.GPIO
import SSM.Frontend.Peripheral.Identity
import SSM.Frontend.Peripheral.BasicBLE

import SSM.Core.Backend
import SSM.Compile
import SSM.Pretty
import SSM.Interpret

import Data.Word

import SSM.Backend.C2.IR

program :: (SupportGPIO backend, SupportBBLE backend) => Compile backend ()
program = do
    (led0, handler0) <- output 0
    (led1, handler1) <- output 1
    (led2, handler2) <- output 2
    input0           <- input  0
    input1           <- input  1
    glo              <- global @Word8
    (_, b, bc, sc)   <- enableBLE

    let ?led0   = led0
        ?led1   = led1
        ?led2   = led2
        ?input0 = input0
        ?input1 = input1
        ?glo    = glo

    schedule main
    schedule handler0
    schedule handler1
    schedule handler2
    schedule b
    schedule bc
    schedule sc
  where
      main :: ( ?led0 :: Ref GPIO , ?led1 :: Ref GPIO, ?led2 :: Ref GPIO
              , ?glo :: Ref Word8, ?input0 :: Ref Switch, ?input1 :: Ref Switch) => SSM ()
      main = routine $ do
          ?led0 <~ high
          ?led1 <~ low
          ?led2 <~ high
          ?input0 <~ deref ?led0
          ?glo  <~ 0


program2 :: Compile backend ()
program2 = schedule test
  where
    test :: SSM ()
    test = routine $ do
      r <- var true
      r1 <- var false
      wait (r, r1)
      fork [ wait r ]


mywait :: Ref a -> SSM ()
mywait r = routine $ wait r

sum :: Ref Word64 -> Ref Word64
    -> Ref Word64 -> SSM ()
sum r1 r2 r = routine $ do
  fork [ mywait r1, mywait r2 ]
  after (secs 1) r (deref r1 + deref r2)

fib :: Exp Word64 -> Ref Word64 -> SSM ()
fib n r = routine $ do
  r1 <- var 0
  r2 <- var 0
  ifThenElse (n <. 2)
    (after (secs 1) r 1)
    (fork [ fib (n-1) r1
          , fib (n-2) r2
          , SSM.Test.sum r1 r2 r
          ])

main2 :: SSM ()
main2 = routine $ do
  r <- var 0
  fork [ fib 4 r ]

program3 :: Compile backend ()
program3 = schedule main2

program4 :: Compile backend ()
program4 = schedule main3

main3 :: SSM ()
main3 = routine $ do
  x <- var $ u64 0
  y <- var $ deref x
  x <~ 5

program5 :: Compile backend ()
program5 = schedule main4

main4 :: SSM ()
main4 = routine $ do
  x <- var $ i32 maxBound
  x <~ deref x + 1
