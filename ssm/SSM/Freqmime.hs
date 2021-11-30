{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
--{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
module SSM.Freqmime where

import           SSM.Core.Type

import           SSM.Compile
import           SSM.Language
import           SSM.Pretty
import qualified SSM.Core as C

import           Data.Word

import           SSM.Frontend.Peripheral.BasicBLE
import           SSM.Frontend.Peripheral.GPIO
import           SSM.Frontend.Peripheral.Identity
import           SSM.Frontend.Peripheral.LED

-- gate_period :: Exp Time
-- gate_period = secs 1

-- blink_time :: Exp Time
-- blink_time = msecs 100

-- type Frequency = Word64

-- freq_count :: Ref SW -> Ref SW -> Ref Frequency -> SSM ()
-- freq_count =
--     box "freq_count" ["gate", "signal", "freq"] $ \gate signal freq -> do
--         wake  <- var event'
--         count <- var 0
--         while true' $ do
--             ifThen (unchanged gate) $ do
--                 wait gate

--             ifThenElse (changed signal) (count <~ 1) (count <~ 0)
--             after gate_period wake event'

--             doWhile
--                 (do
--                     wait (signal, wake)
--                     count <~ (deref count + 1)
--                 )
--                 (unchanged wake)

--             freq <~ (deref count * time2ns (secs 1 /. gate_period))

-- freq_mime :: Ref Frequency -> Ref LED -> SSM ()
-- freq_mime = box "freq_mime" ["freq", "led_ctl"] $ \freq led_ctl -> do
--     wake <- var event'
--     while true' $ do

--         ifThen (deref freq /=. 0) $ do
--             led_ctl <~ true'
--             after (secs 1 /. (nsecs $ deref freq)) wake event'

--         wait (wake, freq)

-- one_shot :: Ref Frequency -> Ref LED -> SSM ()
-- one_shot = box "one_shot" ["freq", "led_ctl"] $ \freq led_ctl -> do
--     while true' $ do
--         wait led_ctl

--         -- try to calculate delay for when it should turn off
--         -- delay will be stored in @delay@ as nanoseconds
--         delay <- var 0
--         ifThenElse
--             (deref freq /=. 0)
--             (  delay
--             <~ (time2ns $ lift2T min' blink_time (secs 1 // deref freq // 2)
--                )
--             )
--             (delay <~ time2ns blink_time)

--         ifThen (deref led_ctl) $ do
--             after (nsecs $ deref delay) led_ctl false'

-- mmain :: (?sw0::Ref SW, ?sw1::Ref SW, ?led_ctl::Ref Bool) => SSM ()
-- mmain = boxNullary "mmain" $ do
--     freq <- var $ u64 0
--     fork
--         [ freq_count ?sw0 ?sw1 freq
--         , freq_mime freq ?led_ctl
--         , one_shot freq ?led_ctl
--         ]

-- testprogram :: Compile ()
-- testprogram = do
--     x            <- switch 0
--     y            <- switch 1
--     (z, handler) <- onoffLED 0

--     let ?sw0     = x
--         ?sw1     = y
--         ?led_ctl = z

--     schedule mmain
--     schedule handler






mmmain :: Compile ()
mmmain = do
    (x, handler) <- onoffLED 0

    let ?led = x

    schedule handler
    schedule mmain
  where
    mmain :: (?led::Ref LED) => SSM ()
    mmain = boxNullary "mmain" $ do
        while true $ do
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
    prog :: (?x::Ref Word8, ?y::Ref Word64) => SSM ()
    prog = boxNullary "prog" $ do
        ?x <~ 5
        ?y <~ 10

{- input: 8 bytes
   layout (where bit 1 = lowest 8 bits)

   1. ID of sender
   2. ID of recipient
   3-8. data

-}

message :: Exp Word64 -> Exp Word64 -> Exp Word64 -> Exp Word64 -> Exp Word64
message sender recipient payload ab = packMessage sender recipient $ attachAB payload ab

unmessage :: Exp Word64 -> (Exp Word64, Exp Word64, Exp Word64, Exp Word64)
unmessage msg = let (from, to, payload) = unpackMessage msg
                    (msg', ab)          = splitMessage payload
                in (from, to, msg', ab)

{- | Pack a message and prepare it for transmission. Arguments are

  1. ID of sender
  2. ID of intended recipient
  3. Payload (max 5 bytes)
-}
packMessage :: Exp Word64 -> Exp Word64 -> Exp Word64 -> Exp Word64
packMessage sender recipient payload = (payload <<. 16) .|. (recipient <<. 8) .|. sender

{- | Unpack a message that was received. Result is a triple with

  1. ID of sender
  2. ID of intended recipient
  3. Payload (max 5 bytes)
-}
unpackMessage :: Exp Word64 -> (Exp Word64, Exp Word64, Exp Word64)
unpackMessage msg = (msg .&. 0x000000ff, (msg .&. 0x0000ff00) >>. 8, msg >>. 16)

{- | If the received messages uses the alternating bit protocol, the first byte
will make up the alternating bit. This function returns a tuple with

  1. Payload
  2. Alternating bit
-}
splitMessage :: Exp Word64 -> (Exp Word64, Exp Word64)
splitMessage msg =
    ((msg .&. 0x0000ff00) >>. 8, msg .&. 0x000000ff)

{- | This function takes a payload (max 7 bytes) and an alternating bit, and produces
a message that can be broadcasted with the alternating bit attached. -}
attachAB :: Exp Word64 -> Exp Word64 -> Exp Word64
attachAB msg ab = (msg <<. 8) .|. ab

ack0 :: Exp Word64
ack0 = 0

ack1 :: Exp Word64
ack1 = 1

{- | Send a message and wait for confirmation. Arguments are:

  1. ID of sender
  2. ID of recipient
  3. Payload (max 4 bytes)
  4. Alternating bit
-}
sendMsg :: (?ble :: BBLE)
        => Exp Word64
        -> Exp Word64
        -> Exp Word64
        -> Exp Word64
        -> SSM ()
sendMsg = box "sendMsg" ["from", "to", "msg", "ab"] $ \source dest msg ab -> do
    -- start broadcasting
    disableBroadcast
    enableBroadcast $ packMessage source dest $ attachAB msg ab

    -- start scanning for confirmation
    enableScan
    doWhile
      (wait scanref)
      (msgIsForMe source dest ab)

    -- after received confirmation, stop scanning
    disableScan
  where
    msgIsForMe :: Exp Word64 -> Exp Word64 -> Exp Word64 -> Exp Bool
    msgIsForMe source dest ab =
       let (from, to, msg, ab') = unmessage $ deref scanref
       in not' $ to ==. source &&. from ==. dest &&. ab ==. ab'

{- | Send a message and wait for confirmation. Arguments are:

  1. ID of sender
  2. ID of recipient
  3. Payload (max 4 bytes)
  4. Alternating bit
-}
acknowledge :: (?ble :: BBLE)
        => Exp Word64
        -> Exp Word64
        -> Exp Word64
        -> Exp Word64
        -> SSM ()
acknowledge = box "acknowledge" ["from", "to", "msg", "ab"] $ \source dest msg ab -> do
    -- start broadcasting
    disableBroadcast
    enableBroadcast $ packMessage source dest $ attachAB msg ab

    -- start scanning for confirmation
    -- we assume we are acknowledged when we get a message with another ab
    enableScan
    doWhile
      (wait scanref)
      (let (from, to, msg, ab') = unmessage $ deref scanref
       in not' $ to ==. source &&. from ==. dest &&. ab /=. ab')

    -- after received confirmation, stop scanning
    disableScan


{-****** Device 1 (the source) ******-}

{- | This code is meant to be run on the device that outputs the actual controls.
Arguments are

  1. ID of this source device
  2. ID of the next device
-}
source :: Exp Word64 -> Exp Word64 -> Compile ()
source this next = do
    (ble, broadcast, scanning) <- enableBasicBLE
    let ?ble = ble

    schedule sourceCommunication
    schedule broadcast
    schedule scanning
  where
      sourceCommunication :: (?ble :: BBLE) => SSM ()
      sourceCommunication = boxNullary "sourceCommunication" $ do
          while true $ do
              fork [sendMsg this next 0 ack0]
              fork [sendMsg this next 1 ack1]
              fork [sendMsg this next 2 ack0]
              fork [sendMsg this next 3 ack1]

{-****** Device 2 & 3 ******-}

{- | This code is meant to be ran on either board 2 or 3, the 'intermediary'
boards that are just responsible for relaying the messages.
Arguments are:

  1. ID of this device
  2. ID of previous device
  3. ID of next device
-}
relay :: Exp Word64 -> Exp Word64 -> Exp Word64 -> Compile ()
relay this previous next = do
    (ble, broadcast, scanning) <- enableBasicBLE
    let ?ble = ble

    schedule relayMessage
    schedule broadcast
    schedule scanning
  where
      relayMessage :: (?ble :: BBLE) => SSM ()
      relayMessage = boxNullary "relayMessage" $ do
          enableScan
          while true $ do
               -- wait for a message
              doWhile
                (wait scanref)
                (let (from, to, msg, ab) = unmessage $ deref scanref
                 in not' $ from ==. previous &&. to ==. this)

              -- grab message contents
              let (_,_,msg, ab) = unmessage $ deref scanref

              -- retransmit and block until acknowledged
              fork [sendMsg this next msg ab]
              -- acknowledge receiving to previous node
              fork [acknowledge this previous 0 ab]

{-****** Devie 4 (the sink) ******-}

sink :: Exp Word64 -> Compile ()
sink this = do
    (ble, broadcast, scanning) <- enableBasicBLE

    (led0, lh0)                <- onoffLED 0
    (led1, lh1)                <- onoffLED 1
    (led2, lh2)                <- onoffLED 2
    (led3, lh3)                <- onoffLED 3

    let ?ble  = ble
        ?led0 = led0
        ?led1 = led1
        ?led2 = led2
        ?led3 = led3

    schedule theSink
    schedule lh0
    schedule lh1
    schedule lh2
    schedule lh3
    schedule broadcast
    schedule scanning
  where
      theSink :: ( ?ble  :: BBLE
                 , ?led0 :: Ref LED
                 , ?led1 :: Ref LED
                 , ?led2 :: Ref LED
                 , ?led3 :: Ref LED
                 ) => SSM ()
      theSink = boxNullary "theSink" $ do
          while true $ do
              enableScan

              -- wait to receive message
              doWhile
                (wait scanref)
                (let (from, to, msg, ab) = unmessage $ deref scanref
                 in not' $ to ==. this)
              disableScan

              -- unpack message info
              let (from, to, msg, ab) = unmessage $ deref scanref

              -- blink led and wait for 1 sec
              switchCase msg
                [ (0, fork [ledBlinker ?led0])
                , (1, fork [ledBlinker ?led1])
                , (2, fork [ledBlinker ?led2])
                , (3, fork [ledBlinker ?led3])
                ]

              -- acknowledge message
              fork [acknowledge this from 0 ab]

      ledBlinker :: Ref LED -> SSM ()
      ledBlinker = box "ledBlinker" ["led"] $ \led -> do
          assign led on
          after (secs 1) led off
          wait led

switchCase :: SSMType a => Exp a -> [(Exp a, SSM ())] -> SSM ()
switchCase _ [] = return ()
switchCase test ((cond, br) : bs) =
    ifThenElse (test ==. cond) br (switchCase test bs)

switchCaseD :: SSMType a => Exp a -> [(Exp a, SSM ())] -> SSM () -> SSM ()
switchCaseD _ [] _ = return ()
switchCaseD test [(cond, br)] def =
  ifThenElse (test ==. cond) br def
switchCaseD test ((cond, br) : bs) def =
  ifThenElse (test ==. cond) br (switchCaseD test bs def)

-- add in paper draft for Stephen
-- also include box version, and partially boxed
delay :: Exp Time -> SSM ()
delay x = do
  wake <- var event
  after x wake event
  wait wake





test1 :: Ref Word64 -> Ref Word64 -> SSM ()
test1 = box "test1" ["x","y"] $ \x y -> do
  assign x (deref y)
  fork [test1 x y]

test2 :: SSM ()
test2 = boxNullary "test2" $ do
  x <- var 0
  y <- var 1
  fork [test1 x y]






buttonBlinky :: Compile ()
buttonBlinky = do
  button <- switch 0
  (led, ledHandler) <- onoffLED 0

  let ?led = led
      ?button = button

  schedule program
  schedule ledHandler
  where


    program :: (?led :: Ref LED, ?button :: Ref SW) => SSM ()
    program = boxNullary "program" $ do
      while true $ do
        wait ?button
        ?led <~ deref ?button
