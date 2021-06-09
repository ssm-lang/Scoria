module HW.Board where

import SSM

--data Board -- some representation of a board and the peripherals on it
           -- such as leds, buttons, uart, BT.



























--nrf52840dk :: SSM Board
--nrf52840dk = undefined

-- | This call makes sure that the code to initialize & use LEDs is executed
-- Since our AST is untyped we can create a value that represents the properties
-- of a led, and then pretend that it's a normal reference to a boolean.
getLed :: Int -> SSM (Ref Bool)
getLed = undefined

f :: Ref Bool -> SSM ()
f r = after 1 r true'
{-
do -- this call performs all the necessary boilerplate and returns
   -- a reference that can access the led
   led <- getLed 1 -- led :: Ref Bool

   -- this call returns a reference to the already created led
   led' <- getLed 1

   -- while this does what the first call to `getLed 1 board` did, but for
   -- another led
   led'' <- getLed 2 board

  -- in the EDSL we use the reference as any 'normal' reference. The fact that
  -- it manages some underlying resource is not a detail we want to clutter
  -- the code with.
  after 50 led true'
  wait [led]
  after 50 led false'
  wait [led]
-}




































{-

typedef struct {
  // all normal fields
  ll_driver_t led_driver;
  bool value;
} sv_led_t;

assign_led(sv_t *var, bool value) {
    sv_led_t* led = (sv_led_t*) var;
    led->value = value;
    ll_write_led(led->led_driver, led->value);
}

update_led(sv_t* var) {
    sv_led_t* led = (sv_led_t*) var;
    led->value = led->event_value;
    ll_write_led(led->led_driver, led->value);
}


void main(void) {
  // as soon as we need to interact with the
  // board in any way, generate this code in main
  ll_driver_init(..); // initiate ll driver
  
  // if you require `led0` from the board, first make sure to
  // generate this code to initialize the HW LED
  driver_t led0;
  ll_init_led(&led0);

  // Then create and initialize the scheduler variable
  sv_led_t led0v;
  initialize_led(&led0v);
  led0v.led_driver = led0;
  assign_led(&led0, false);

}

-}
