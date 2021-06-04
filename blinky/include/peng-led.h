#ifndef PENG_LED_
#define PENG_LED_

#include <zephyr.h>

#include <stdbool.h>
#include <stdio.h>

#include <peng.h>

#include <ll/ll_driver.h>
#include <ll/ll_led.h>

/********** This code will be generated when LED IO is required **********/

/*
 * LED variable
 */
typedef struct {
  /* Generic SV fields */
  void (*update)(sv_t *);
  struct trigger *triggers;
  peng_time_t last_updated;
  peng_time_t event_time;
  void (*to_string)(sv_t *, char *, size_t);

  /* LED specific fields */
  ll_driver_t driver; // driver used to toggle leds
  /* For this example we use a bool to model LED state, but we want our
   * own ADT type here later.
   */
  bool value;
  bool event_value;
} sv_led_t;

void to_string_led(sv_t *v, char *buffer, size_t size);
void initialize_led(sv_led_t *v, uint32_t ledid); // NOTE: Extra field here
void assign_led(sv_led_t *v,  priority_t priority, bool value);
void later_led(sv_led_t *v, peng_time_t time, bool value);
void update_led(sv_t *var);

#endif // PENG_LED