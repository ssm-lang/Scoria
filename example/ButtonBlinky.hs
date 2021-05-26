module ButtonBlinky where

import Data.Word
import SSM

-- | This is what we will investigate next, how to implement a function
-- like this one.
getButtons :: SSM (Ref Word8, Ref Word8, Ref Word8, Ref Word8)
getButtons = undefined

-- | Likewise with this one.
greenLed :: SSM (Ref Bool)
greenLed = undefined

redLed :: SSM (Ref Bool)
redLed = undefined

blueLed :: SSM (Ref Bool)
blueLed = undefined

enableLed :: Ref Bool -> SSM ()
enableLed led = led <~ true'

disableLed :: Ref Bool -> SSM ()
disableLed led = led <~ false'
{-
buttonblinky :: SSM ()
buttonblinky = box "buttonblinky" [] $ do
    (but1, but2, but3, but4) <- getButtons
    green <- greenLed
    red   <- redLed
    blue  <- blueLed
    disableLed green
    disableLed red
    disableLed blue

    wait [but1, but2, but3, but4]
    b <- changed but1
    c <- changed but3

    ifThenElse b
      (do wait [but2, but3, but4]
          b <- changed but2
          ifThenElse b
            (enableLed green)
            (enableLed blue))
      (ifThenElse c
        (do wait [but1, but2, but4]
            b <- changed but4
            ifThenElse b
              (enableLed red)
              (enableLed blue))
        (enableLed blueLed))
-}
{-
There is not really a reason why changed should have the type `Ref a -> SSM Bool`,
if I change it to return `Exp Bool` instead, we can make it slightly more compact like this.

buttonblinky :: SSM ()
buttonblinky = box "buttonblinky" [] $ do
    (but1, but2, but3, but4) <- getButtons
    led <- greenLed

    disableLed led
    wait [but1, but3]
    b <- changed but1

    ifThenElse (changed but1)
      (waitAndEnable but2)
      (waitAndEnable but4)
  where
      waitAndEnable :: Ref a -> Ref Bool -> SSM ()
      waitAndEnable = box "waitAndEnable" ["r","led"] $ \r led ->
        wait [r]
        enableLed led
-}