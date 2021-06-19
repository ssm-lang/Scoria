{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImplicitParams #-}
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
import LowInterpreter
import Trace
import LowPretty

import Control.Monad.State
import qualified Data.Map as Map
import Data.Int
import Data.Maybe

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

-- | The Compile state records which peripherals are accessed.
data ST = ST
  { ledperipheral :: Maybe LEDPeripheral
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
            let ledref = Ptr ("led" ++ show i, Ref (Special "led"))
            let ledrefs' = Map.insert i ledref $ ledrefs ledp
            modify $ \st -> st { ledperipheral = Just $ ledp { ledrefs = ledrefs' } }
            return ledref

data IOPeripherals = IOP
  { leds :: [Ref LED]
  }

led :: Int -> IOPeripherals -> SSM (Ref LED)
led i iop = return $ leds iop !! i




class GenC a where
  -- | Header file to generate
  header     :: a -> Maybe (String, [C.Definition])
  -- | Source file to generate
  source     :: a -> Maybe (String, [C.Definition])
  -- | Initialize IO code to inline in the initialize function
  initialize :: a -> Maybe [C.BlockItem]
  -- | Global definitions
  globals    :: a -> Maybe [C.Definition]
  -- | Code to run in main, if any
  mainInit   :: a -> Maybe [C.Definition]

instance GenC LEDPeripheral where
  header     _  = Just ("peng-led.h", ledHeader)
  source     _  = Just ("peng-led.c", ledSource)
  initialize lp = Just $ ledInit lp
  globals    lp = Just $ ledGlobals lp
  mainInit   _  = Nothing

sv_t :: C.Type
sv_t = [cty| typename sv_t |]

sv_led_t :: C.Type
sv_led_t = [cty| typename sv_led_t |]

peng_time_t :: C.Type
peng_time_t = [cty| typename peng_time_t |]

priority_t :: C.Type
priority_t = [cty| typename priority_t |]

size_t :: C.Type
size_t = [cty| typename size_t |]

act_t :: C.Type
act_t = [cty| typename act_t |]

uint8_t :: C.Type
uint8_t = [cty| typename size_t |]

uint32_t :: C.Type
uint32_t = [cty| typename uint32_t |]

uint64_t :: C.Type
uint64_t = [cty| typename uint64_t |]

bool :: C.Type
bool = [cty| typename bool |]

ll_driver_t :: C.Type
ll_driver_t = [cty| typename ll_driver_t |]

ledGlobals :: LEDPeripheral -> [C.Definition]
ledGlobals ledp = concat $ map (\n -> [cunit| $ty:sv_led_t $id:n; |]) $ lednames
  where
    lednames = map (\(Ptr (r,_)) -> r) $ Map.elems (ledrefs ledp)

ledInit :: LEDPeripheral -> [C.BlockItem]
ledInit ledp = concatMap stmts leds
  where
    leds = map (\(i, Ptr (r,_)) -> (r,i)) $ Map.toList (ledrefs ledp)

    stmts (n,i) = [ [citem| initialize_led(&$id:n, $int:i); |]
                  , [citem| $id:n.value = false; |]
                  ]

ledHeader :: [C.Definition]
ledHeader =
  [ [cedecl| $esc:("#ifndef PENG_LED_H") |]
  , [cedecl| $esc:("#define PENG_LED_H") |]

  , [cedecl| $esc:("#include <stdbool.h>")      |]
  , [cedecl| $esc:("#include <stdio.h>")        |]
  , [cedecl| $esc:("#include <peng.h>")         |]
  , [cedecl| $esc:("#include <ll/ll_driver.h>") |]
  , [cedecl| $esc:("#include <ll/ll_led.h>")    |]

  , [cedecl| typedef struct {
               /* Generic SV fields */
               void (*update)($ty:sv_t *);
               struct trigger *triggers;
               $ty:peng_time_t last_updated;
               $ty:peng_time_t event_time;
               void (*to_string)($ty:sv_t *, char *, $ty:size_t);

               /* LED specific fields */
               $ty:bool value;
               $ty:bool event_value;
               $ty:ll_driver_t driver;
             } sv_led_t;
    |]
  , [cedecl| void to_string_led($ty:sv_t *v, char *buffer, $ty:size_t size); |]
  , [cedecl| void initialize_led($ty:sv_led_t *v, $ty:uint32_t ledid);|]
  , [cedecl| void assign_led($ty:sv_led_t *v, $ty:priority_t priority, $ty:bool value); |]
  , [cedecl| void later_led($ty:sv_led_t *v, $ty:peng_time_t time, $ty:bool value); |]
  , [cedecl| void update_led($ty:sv_t *var); |]
  , [cedecl| $esc:("#endif // PENG_LED_H") |]
  ]

ledSource :: [C.Definition]
ledSource =
  [ [cedecl| $esc:("#include <zephyr.h>")       |]
  , [cedecl| $esc:("#include <stdbool.h>")      |]
  , [cedecl| $esc:("#include <stdio.h>")        |]
  , [cedecl| $esc:("#include <peng.h>")         |]
  , [cedecl| $esc:("#include <ll/ll_driver.h>") |]
  , [cedecl| $esc:("#include <ll/ll_led.h>")    |]
  , [cedecl| $esc:("peng-led.h")                |]
    
  , [cedecl| void to_string_led($ty:sv_t *v, char *buffer, $ty:size_t size) {
               $ty:sv_led_t* iv = ($ty:sv_led_t *) v;
               char str[] = "led %s";
               snprintf(buffer, size, str, iv->value ? "on" : "off");
               return;
             }
    |]
  , [cedecl| void initialize_led($ty:sv_led_t *v, $ty:uint32_t ledid) {
               assert(v);
               /* Generic initialization */
               *v = ($ty:sv_led_t) { .to_string    = to_string_led
                                     , .update       = update_led
                                     , .triggers     = NULL
                                     , .last_updated = now
                                     , .event_time   = NO_EVENT_SCHEDULED
                                     };
               /* LED specific initialization */
               if(ll_led_init(&v->driver, ledid, 0)) {
                 printk("LED-driver %d successfully initialized\n", ledid);
               } else {
                 printk("LED-driver %d failed to initialize\n", ledid);
               }
             }
    |]
  , [cedecl| void assign_led($ty:sv_led_t *v, $ty:priority_t priority, $ty:bool value) {
               v->value = value;
               v->last_updated = now;
               schedule_sensitive(($ty:sv_t *) v, priority);
               
               /* After performing the assignment, reflect the new value in the driver */
               $ty:uint8_t cmd = v->value ? 1 : 0;
               ll_write(&v->driver, &cmd, 1);
             }
    |]
  , [cedecl| void later_led($ty:sv_led_t *v, $ty:peng_time_t time, $ty:bool value) {
               assert(v);
               v->event_value = value;
               later_event(($ty:sv_t *) v, time);
             }
    |]
  , [cedecl| void update_led($ty:sv_t *var) {
               assert(var);
               assert(var->event_time == now);
               $ty:sv_led_t *v = ($ty:sv_led_t *) var;
               v->value = v->event_value;

               /* After updating the value, reflect the new value in the driver */
               $ty:uint8_t cmd = v->value ? 1 : 0;
               ll_write(&v->driver, &cmd, 1);
             }
    |]
  ]

makeInitializeFunction :: [C.BlockItem] -> C.Definition
makeInitializeFunction items = [cedecl| void initializeIO() { $items:items } |]

ll_driver_msg_t :: C.Type
ll_driver_msg_t = [cty| typename ll_driver_msg_t |]

staticZephyrCode :: [C.Definition]
                 -> C.Definition
                 -> C.Definition
                 -> [C.Definition]
staticZephyrCode globaldecls ioinit continit =
  [ [cedecl| $esc:("#include <zephyr.h>")                |]
  , [cedecl| $esc:("#include <drivers/counter.h>")       |]
  , [cedecl| $esc:("#include <zephyr/types.h>")          |]
  , [cedecl| $esc:("#include <stddef.h>")                |]
  , [cedecl| $esc:("#include <ll/ll_driver.h>")          |]
  , [cedecl| $esc:("#include <ll/ll_led.h>")             |]
  , [cedecl| $esc:("#include <hal/zephyr/svm_zephyr.h>") |]
  , [cedecl| $esc:("#include <stdbool.h>")               |]
  , [cedecl| $esc:("#include <stdio.h>")                 |]
  , [cedecl| $esc:("#include <peng.h>")                  |]
  , [cedecl| $esc:("#include <peng-led.h>")              |]
  , [cedecl| $esc:("#include <peng-program.h>")          |]
  ] ++ globaldecls ++
  [ ioinit
  , [cedecl| void top_return($ty:act_t *act) {
               return;
             }
    |]
  , [cedecl| $ty:act_t top = { .step = top_return }; |]
  , continit
  , [cedecl| $esc:("K_MSG_DEFINE(tick_msgq, sizeof($ty:ll_driver_msg_t), 100, 1);") |]
  , [cedecl| struct counter_alarm_cfg alarm_cfg; |]
  , [cedecl| const struct device *counter_dev = NULL; |]
  , [cedecl| void tick_thread_main(void *a, void *b, void *c) {
               void(a);
               void(b);
               void(c);

               initialize_IO();
               initialize_cont();

               now = 0;
               $ty:ll_driver_msg_t recv_msg;

               while(1) {

                 k_msgq_get(&tick_msgq, &recv_msg, K_FOREVER);

                 switch(recv_msg.msg_type) {
                   case 0:
                     break;
                   default:
                     printk("default case - type: %d\n", recv_msg.msg_type);
                 }

                 now = next_event_time();
                 tick();

                 $ty:peng_time_t next = next_event_time();

                 if(next == ULLONG_MAX) {
                   printk("NOTHING IN TE QUEUE\r\n");
                   continue;
                 }

                 $ty:uint64_t wake_time = next_event_time(); /* Absolute time */
                 alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
                 alarm_cfg.ticks = wake_time;
                 int r = counter_set_channel_alarm(counter_dev, 0, &alarm_cfg);
                 if (!r) {
                 } else {
                   if (r == - ENOTSUP ) {
                     printk("hw_tick: Error setting alarm (ENOTSUP)\r\n");
                   } else if ( r == - EINVAL ) {
                     printk("hw_tick: Error setting alarm (EINVAL)\r\n");
                   } else if ( r == - ETIME ) {
                     printk("hw_tick: Error setting alarm (ETIME)\r\n");
                   } else {
                     printk("hw_tick: Error setting alarm\r\n");
                   }
                 }
               }
               return;
             }
    |]
  , [cedecl| $esc:("K_THREAD_STACK_DEFINE(tick_thread_stack, 512);")|]
  , [cedecl| struct k_thread tick_thread;|]
  , [cedecl| void start_tick_thread(void) {
               k_thread_create( &tick_thread
                              , tick_thread_stack
                              , K_THREAD_STACK_SIZEOF(tick_thread_stack)
                              , tick_thread_main
                              , NULL, NULL, NULL
                              , 5, 0, K_NO_WAIT);
             }
    |]
  , [cedecl| void hw_tick(const struct device *dev, $ty:uint8_t chan, $ty:uint32_t ticks, void *user_data) {
               $ty:ll_driver_msg_t msg = { .driver_id = -1
                                         , .msg_type  = 1
                                         , .data      = 0
                                         , .timestamp = 0
                                         };
               k_msgq_put(&tick_msgq, &msg, K_NO_WAIT);
             }
    |]
  , [cedecl| void main(void) {
               printk("Sleeping 1 seconds\r\n");
               k_sleep(K_SECONDS(1));
               printk("Woke up\r\n");

               counter_dev = device_get_binding(DT_LABEL(DT_ALIAS(ssm_timer)));
               if(!counter_dev) {
                 printk("HWCounter: Device not found error\r\n");
               }

               if(cunter_get_frequency(counter_dev) > 1000000) {
                 printk("HWCounter: Running at %dMHz\r\n", counter_get_frequency(counter_dev) / 1000000);
               } else {
                 printk("HWCounter: Running at %dHz\r\n", counter_get_frequency(counter_dev));
               }

               alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
               alarm_cfg.ticks = 10;
               alarm_cfg.callback = hw_tick;
               alarm_cfg.user_data = &alarm_cfg;

               if (!counter_set_channel_alarm(counter_dev, 0, &alarm_cfg)) {
                 printk("HWCounter: Alarm set\r\n");
               } else {
                 printk("HWCounter: Error setting alarm\r\n");
               }
               if (!counter_set_guard_period(counter_dev, UINT_MAX/2, COUNTER_GUARD_PERIOD_LATE_TO_SET)) {
                 printk("HWCounter: Guard period set\r\n");
               } else {
                 printk("HWCounter: Error setting guard period\r\n");
               }

               counter_start(counter_dev);
               printk("Starting Tick-Thread\r\n");
               start_tick_thread();
             }
    |]
  ]




testprogram :: (?io :: IOPeripherals) => SSM ()
testprogram = boxNullary "testprogram" $ do
  led0 <- led 0 ?io
  while' true' $ do
    after 50 led0 true'
    Frontend.wait [led0]
    after 50 led0 false'
    Frontend.wait [led0]

mainSSM :: Compile (SSM ())
mainSSM = do
  led0 <- getLED 0
  led1 <- getLED 1

  let ?io = IOP [led0, led1] in return testprogram

handle :: IO ()
handle = do
  let (ssm,st) = runState mainSSM $ ST Nothing
  putStrLn $ pretty 120 $ pprList $ snd $ fromJust $ header $ fromJust $ ledperipheral st
  putStrLn $ pretty 120 $ pprList $ snd $ fromJust $ source $ fromJust $ ledperipheral st
  putStrLn $ pretty 120 $ pprList $ fromJust $ globals $ fromJust $ (ledperipheral st)
  let blockitems = initialize $ fromJust $ ledperipheral st
  putStrLn $ pretty 120 $ ppr $ makeInitializeFunction $ fromJust blockitems
  putStrLn $ compile False Nothing ssm