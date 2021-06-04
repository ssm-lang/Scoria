#ifndef PENG_BUTTON_H_
#define PENG_BUTTON_H_

#include <zephyr.h>

#include <stdbool.h>
#include <stdio.h>

#include <peng.h>

#include <ll/ll_driver.h>
#include <ll/ll_button.h>

/********** This code will be generated when button IO is required **********/

/*
 * Button variable
 */
typedef struct {
  /* Generic SV fields */
  void (*update)(sv_t *);
  struct trigger *triggers;
  peng_time_t last_updated;
  peng_time_t event_time;
  void (*to_string)(sv_t *, char *, size_t);

  /* Button specific fields */
  ll_driver_t driver;
  bool value; // again, using a boolean to simulate button states. Want ADT types here.
  bool event_value;
} sv_button_t;

/*
 * The compiler will statically know how many different buttons will be
 * interacted with, and will allocate an array big enough to hold the buttons.
 */
extern sv_button_t buttons[4];

void to_string_button(sv_t *v, char *buffer, size_t size);
void initialize_button(sv_button_t *v, uint32_t driverid, uint32_t buttonid); // NOTE: Extra fields here
void assign_button(sv_button_t *v,  priority_t priority, bool value);
void later_button(sv_button_t *v, peng_time_t time, bool value);
void update_button(sv_t *var);

#endif // PENG_BUTTON_H