{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GADTs #-}
module SSM
  ( Ref
  , Exp
  , Lit
  , inputref
  , (+.)
  , (-.)
  , (*.)
  , (<.)
  , (==.)
  , (<~)
  , Frontend.neg
  , int32
  , int64
  , uint64
  , word8
  , true'
  , false'
  , deref
  , var
  , Frontend.wait
  , waitAll
  , after
  , Frontend.fork
  , changed
  , if'
  , ifThen
  , ifThenElse
  , while'
  , SSM
  , Program
  , box
  , boxNullary
  , Output
  , SSM.compile
  , SSM.interpret
  , prettyPrint
) where

import Frontend
import LowCodeGen hiding (sv_t, priority_t, act_t)
import LowCore
import Core
import LowInterpreter hiding (wait)
import Trace
import LowPretty

import Control.Monad.State
import qualified Data.Map as Map
import Data.Int
import Data.Maybe
import Data.Either
import Data.List

import           Text.PrettyPrint.Mainland      ( pretty )
import           Text.PrettyPrint.Mainland.Class
                                                ( pprList, ppr )
import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

compile :: Bool -> Maybe Int -> SSM () -> String
compile b me p = compile_ b me $ transpile p

interpret :: SSM () -> Output
interpret = LowInterpreter.interpret . transpile

prettyPrint :: SSM () -> String
prettyPrint = prettyProgram . transpile







-- | LEDs can take the same states as a Boolean
type LED = Bool

-- | The LED peripheral maintains information of which LEDs were
-- initialized, and what the references are.
data LEDPeripheral = LEDPeripheral
  { ledrefs :: Map.Map Int (Ref LED)
  }

emptyLEDPeripheral :: LEDPeripheral
emptyLEDPeripheral = LEDPeripheral Map.empty

type Button = Bool

data ButtonPeripheral = ButtonPeripheral
  { nextDriverID :: Int                         -- Next available driver ID
  , buttonrefs   :: Map.Map Int (Int, Ref Button)  -- Button ID -> (Driver ID, Button reference)
  }

emptyButtonPeripheral :: ButtonPeripheral
emptyButtonPeripheral = ButtonPeripheral 0 Map.empty

-- | The Compile state records which peripherals are accessed.
data ST = ST
  { ledperipheral    :: Maybe LEDPeripheral
  , buttonperipheral :: Maybe ButtonPeripheral
  }

-- | Compile monad
type Compile a = State ST a

-- | Get a LED from the Compile monad
getLED :: Int -> Compile (Ref LED)
getLED i | i < 0 || i > 3 = error "invalid LED identifier"
getLED i = do
  mledp <- gets ledperipheral
  case mledp of
    Just ledp -> createLed ledp
    Nothing   -> createLed emptyLEDPeripheral
  where
      createLed :: LEDPeripheral -> Compile (Ref LED)
      createLed ledp = do
        case Map.lookup i (ledrefs ledp) of
          Just ref -> return ref
          Nothing  -> do
            let ledref   = Ptr $ Static $ ("led" ++ show i, Ref (Special "led"))
                ledrefs' = Map.insert i ledref $ ledrefs ledp
            modify $ \st -> st { ledperipheral = Just $ ledp { ledrefs = ledrefs' } }
            return ledref

-- | Get a button from the Compile monad
getButton :: Int -> Compile (Ref Button)
getButton i | i < 0 || i > 3 = error "invalid button identifier"
getButton i = do
  mbuttonp <- gets buttonperipheral
  case mbuttonp of
    Just buttonp -> createButton buttonp
    Nothing      -> createButton emptyButtonPeripheral
  where
      createButton :: ButtonPeripheral -> Compile (Ref Button)
      createButton buttonp = do
        case Map.lookup i (buttonrefs buttonp) of
          Just (_,ref) -> return ref
          Nothing      -> do
            let driverid    = nextDriverID buttonp
                buttonref   = Ptr $ Static ("button" ++ show i, Ref (Special "button"))
                buttonrefs' = Map.insert i (driverid, buttonref) $ buttonrefs buttonp
            modify $ \st -> st
              { buttonperipheral = Just $ buttonp { nextDriverID = driverid + 1
                                                  , buttonrefs   = buttonrefs'
                                                  }
              }
            return buttonref


type IOLED    = [Ref LED]
type IOButton = [Ref Button]

led :: (?ioleds :: IOLED) => Int -> Ref LED
led i = ?ioleds !! i

button :: (?iobuttons :: IOButton) => Int -> Ref Button
button i = ?iobuttons !! i

class GenC a where
  -- | Header files to include
  include    :: a -> C.Definition
  -- | Initialize IO code to inline in the initialize function
  initialize :: a -> Maybe [C.BlockItem]
  -- | Global definitions
  globals    :: a -> Maybe [C.Definition]
  -- | Code to run in main, if any
  mainInit   :: a -> Maybe [C.BlockItem]
  -- | How to handle incoming driver messages
  handleDriver :: a -> Maybe [C.BlockItem]

sv_led_t :: C.Type
sv_led_t = [cty| typename sv_led_t |]

sv_button_t :: C.Type
sv_button_t = [cty| typename sv_button_t |]

peng_time_t :: C.Type
peng_time_t = [cty| typename peng_time_t |]

act_t :: C.Type
act_t = [cty| typename act_t |]

uint8_t :: C.Type
uint8_t = [cty| typename uint8_t |]

uint32_t :: C.Type
uint32_t = [cty| typename uint32_t |]

uint64_t :: C.Type
uint64_t = [cty| typename uint64_t |]

{-********** Start of LED C Code **********-}

instance GenC LEDPeripheral where
  include    _   = [cedecl| $esc:("#include <peng-led.h>")|]
  initialize lp  = Just $ ledInit lp
  globals    lp  = Just $ ledGlobals lp
  mainInit   _   = Nothing
  handleDriver _ = Nothing

ledGlobals :: LEDPeripheral -> [C.Definition]
ledGlobals ledp = concat $ map (\n -> [cunit| $ty:sv_led_t $id:n; |]) $ lednames
  where
    lednames = map (\(Ptr r) -> refName r) $ Map.elems (ledrefs ledp)

ledInit :: LEDPeripheral -> [C.BlockItem]
ledInit ledp = concatMap stmts leds
  where
    leds = map (\(i, Ptr r) -> (refName r,i)) $ Map.toList (ledrefs ledp)

    stmts (n,i) = [ [citem| initialize_ledIO(&$id:n, $int:i); |]
                  , [citem| $id:n.value = false; |]
                  ]

{-********** End of LED C Code ***********-}

{-********** Start of Button C Code **********-}
zephyr_interop_t :: C.Type
zephyr_interop_t = [cty| typename zephyr_interop_t |]

instance GenC ButtonPeripheral where
  include    _    = [cedecl| $esc:("#include <peng-button.h>")|]
  initialize bp   = Just $ buttonInit bp
  globals    bp   = Just $ buttonGlobals bp
  mainInit   _    = Nothing
  handleDriver bp = Just $ buttonHandleDriver bp

-- citem
buttonInit :: ButtonPeripheral -> [C.BlockItem]
buttonInit bp = concat $ map initbutton $ Map.toList (buttonrefs bp)
  where
      initbutton :: (Int, (Int, Ref Button)) -> [C.BlockItem]
      initbutton (buttonid, (driverid, (Ptr r))) =
        [ [citem| initialize_buttonIO(&$id:(refName r), $int:driverid, $int:buttonid); |]
        , [citem| $id:(refName r).value  = false; |]
        , [citem| buttons[$int:driverid] = &$id:(refName r);|]
        ]

buttonGlobals :: ButtonPeripheral -> [C.Definition]
buttonGlobals bp =
  [ [cedecl| $ty:sv_button_t *buttons[$int:(Map.size (buttonrefs bp))];|]
  ] ++ map buttondecl (Map.toList $ buttonrefs bp)
  where
    buttondecl :: (Int, (Int, Ref Button)) -> C.Definition
    buttondecl (driverid, (buttonid, (Ptr r))) =
      [cedecl| $ty:sv_button_t $id:(refName r); |]

buttonHandleDriver :: ButtonPeripheral -> [C.BlockItem]
buttonHandleDriver bp =
  [ [citem| if(msg->msg_type == 1 && ($exp:checkDriverID)) {
              $ty:uint64_t hw_now;
              get_hw_now(&hw_now);

              /* Fetch button variable from buttons schedule it in 1 tick */
              $ty:sv_button_t* button = buttons[msg->driver_id];
              later_button(button, hw_now, msg->data);
            }
  |] ]
  where
    driverIDs :: [Int]
    driverIDs = map fst $ Map.elems (buttonrefs bp)

    checkDriverID :: C.Exp
    checkDriverID =
      foldl
        (\curr id -> [cexp| $exp:curr || msg->driver_id == $int:id|])
        [cexp|false|]
        driverIDs
{-********** End of Button C Code **********-}

makeInitializeIOFunction :: [C.BlockItem] -> C.Definition
makeInitializeIOFunction items = [cedecl| void initializeIO() { $items:items } |]

-- | The C-Statements that represent setting up the input arguments
-- and forking the programs entrypoint. Stolen from Johns code in LowCodeGen.
makeInitializeCont :: Program -> [C.Definition]
makeInitializeCont program =
  [ [cedecl| $esc:("extern act_t top;")|]
  , [cedecl| void initializeCont() {
               $id:(LowCodeGen.fork)(($ty:act_t *) $id:enter($args:enterArgs));
             }
    |]
  ]
  where
    enter :: String
    enter = enter_ $ entry program
    
    enterArgs :: [C.Exp]
    enterArgs =
      [ [cexp|($ty:act_t *) &top|]
        , [cexp|PRIORITY_AT_ROOT|]
        , [cexp|DEPTH_AT_ROOT|]
        ]

ll_driver_msg_t :: C.Type
ll_driver_msg_t = [cty| typename ll_driver_msg_t |]

makeHandleMsg :: [C.BlockItem] -> [C.Definition]
makeHandleMsg msgcases =
  [ [cedecl| $esc:("extern void get_hw_now(uint64_t *time);") |]
  , [cedecl| void handle_driver_msg($ty:ll_driver_msg_t *msg) {
               if(msg->msg_type == 0 && msg->driver_id == -1) {
                 printk("woke up, now it's time to call tick\n");
               }
               $items:msgcases
             }
    |]
  ]

-- | Data type to 'hide' what actual type the peripheral has, so we can
-- create a HList
data Peripheral where
  Peripheral :: GenC a => a -> Peripheral

-- | Dummy instance for Peripherals
instance GenC Peripheral where
  include      (Peripheral p) = include p
  initialize   (Peripheral p) = initialize p
  globals      (Peripheral p) = globals p
  handleDriver (Peripheral p) = handleDriver p
  mainInit     (Peripheral p) = mainInit p

stToPeripherals :: ST -> [Peripheral]
stToPeripherals st = concat [ maybe [] (\p -> [Peripheral p]) (ledperipheral st)
                            , maybe [] (\p -> [Peripheral p]) (buttonperipheral st)]

-- | Merge all C-Code that's generated for the peripherals
genPeripheralCode :: [Peripheral]
                  -> ( [C.Definition]  -- All includes required by the peripherals
                     , [C.Definition]  -- All globablly declared variables and other important stuff
                     , [C.BlockItem]   -- All initialization statements (to go into a procedure)
                     , [C.BlockItem]   -- All cases that can handle driver messages
                     , [C.BlockItem]   -- Any code that is meant to run in main before program init
                     )
genPeripheralCode ps =
  let includes          = map include ps
      gbls              = getAll globals    (<>) [] ps
      inits             = getAll initialize (<>) [] ps
      msgcases          = getAll handleDriver (<>) [] ps
      minits            = getAll mainInit   (<>) [] ps
  in (includes, gbls, inits, msgcases, minits)
  where
    getAll :: (a -> Maybe b) -> (c -> b -> c) -> c -> [a] -> c
    getAll f g init = foldl g init . map fromJust . filter isJust . map f

-- | Take a SSM program and a list of peripherals, and return a string
-- that contains all the generated C-COde.
genCFile :: Maybe Int -> SSM () -> [Peripheral] -> String
genCFile mi prg ps = pretty 120 $ pprList compunit
  where
    includes,gbls :: [C.Definition]
    inits,minits         :: [C.BlockItem]
    (includes, gbls, inits, msgcases, minits) = genPeripheralCode ps
    
    ioinit :: C.Definition
    ioinit   = makeInitializeIOFunction inits
    
    prg' :: Program
    prg' = transpile prg

    ssmprog, ssmincludes :: [C.Definition]
    (ssmprog, ssmincludes) = compileCDefs False mi prg'

    continit :: [C.Definition]
    continit = makeInitializeCont prg'

    compunit :: [C.Definition]
    compunit = concat [ (nub (includes <> ssmincludes))
                      , gbls
                      , ssmprog
                      , [ioinit]
                      , continit
                      , makeHandleMsg msgcases
                      ]

totalCompile :: Compile (SSM ()) -> String
totalCompile cunit = let (ssm, st) = runState cunit $ ST Nothing Nothing
                     in genCFile Nothing ssm (stToPeripherals st)






toggle :: Ref LED -> SSM ()
toggle r = Frontend.fork [ toggleprocess r ]
  where
    toggleprocess :: Ref LED -> SSM ()
    toggleprocess = box "toggleprocess" ["r"] $ \r -> do
                      v <- deref r
                      ifThenElse (v ==. true')
                        (r <~ false')
                        (r <~ true')

delay :: Exp Word64 -> SSM ()
delay time = Frontend.fork [delayprocess time]
  where
    delayprocess :: Exp Word64 -> SSM ()
    delayprocess = box "delayprocess" ["time"] $ \time -> do
                    r <- var (0 :: Exp Int32)
                    after time r 1
                    wait [r]

testprogram :: (?ioleds :: IOLED) => SSM ()
testprogram = boxNullary "testprogram" $ do
  while' true' $ do
    toggle $ led 0
    delay 4000000
    toggle $ led 1
    delay 4000000
    toggle $ led 2
    delay 4000000
    toggle $ led 3
    delay 4000000


mainSSM :: Compile (SSM ())
mainSSM = do
  led0 <- getLED 0
  led1 <- getLED 1
  led2 <- getLED 2
  led3 <- getLED 3

  let ?ioleds = [led0, led1, led2, led3]
  return testprogram









-- I am manually using ticks here, but this should be handled by the compiler
-- later on. 16 000 000 = 1 second.

testprogram2 :: ( ?ioleds    :: IOLED
                , ?iobuttons :: IOButton
                )
             => SSM ()
testprogram2 = boxNullary "testprogram2" $ do
  sequence_ [wait [button 0], wait [button 0]]
  delay (5*16000000)
  sequence_ $ concat $ replicate (4*5) [toggle (led 0), delay 4000000]



-- | Boring setup
mainSSM2 :: Compile (SSM ())
mainSSM2 = do
  button0 <- getButton 0
  led0    <- getLED 0
  let ?ioleds    = [led0]
      ?iobuttons = [button0]
  return testprogram2