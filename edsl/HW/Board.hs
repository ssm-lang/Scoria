module Board where

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
