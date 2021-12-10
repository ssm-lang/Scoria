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

import SSM.Core.Backend
import SSM.Compile
import SSM.Pretty

import Data.Word

program :: (SupportGPIO backend) => Compile backend ()
program = do
    (led0, handler0) <- output 0
    (led1, handler1) <- output 1
    (led2, handler2) <- output 2
    glo <- global @Word8

    let ?led0 = led0
        ?led1 = led1
        ?led2 = led2
        ?glo = glo

    schedule main
    schedule handler0
    schedule handler1
    schedule handler2
  where
      main :: (?led0 :: Ref GPIO, ?led1 :: Ref GPIO, ?led2 :: Ref GPIO, ?glo :: Ref Word8) => SSM ()
      main = routine $ do
          ?led0 <~ high
          ?led1 <~ low
          ?led2 <~ high
          ?glo  <~ 0
