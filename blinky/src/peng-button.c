
#include <zephyr.h>

#include <stdbool.h>
#include <stdio.h>

#include <peng.h>

#include <ll/ll_driver.h>
#include <ll/ll_button.h>
#include <hal/zephyr/svm_zephyr.h>

#include <peng-button.h>

extern struct k_msgq tick_msgq;

/********** This code will be generated when button IO is required **********/

/* Function used by the ll-driver to deliver button events */
int send_message(zephyr_interop_t* this, ll_driver_msg_t msg) {
   return k_msgq_put(this->msgq,(void*)&msg, K_NO_WAIT);
}

/* This is part of the ll-driver interface Joel & Abi wrote */
extern int send_message(zephyr_interop_t* this, ll_driver_msg_t msg);
zephyr_interop_t button_interop = { .msgq         = &tick_msgq
                                  , .send_message = send_message
                                  };

void to_string_button(sv_t *v, char *buffer, size_t size) {
    sv_button_t* iv = (sv_button_t *) v;
    char str[] = "button %s";
    snprintf(buffer, size, str, iv->value ? "button-down" : "button-up");
}

/*
* Below follow the type specific functions for buttons. They are not really
 * interesting, apart from the initialization function. We don't want anything
 * special to happen when we assign to a button (this should probably not be
 * allowed by the compiler), and events will go via another route (will explain
 * further down).
 */
void initialize_button(sv_button_t *v, uint32_t driverid, uint32_t buttonid) {
  assert(v);
  /* Generic initialization */
  *v = (sv_button_t) { .to_string = to_string_button,
                    .update       = update_button,
                    .triggers     = NULL,
			              .last_updated = now,
			              .event_time   = NO_EVENT_SCHEDULED };

  /* Button specific initialization */
  if(ll_button_init(&v->driver, driverid, &button_interop, buttonid)) {
      printk("Button-driver %d successfully initialized\n", buttonid);
  } else {
      printk("driver failed to initialize\n");
  }
}

void assign_button(sv_button_t *v,  priority_t priority, bool value) {
  v->value = value;
  v->last_updated = now;
  schedule_sensitive((sv_t *) v, priority);
}

void later_button(sv_button_t *v, peng_time_t time, bool value) {
  assert(v);
  v->event_value = value;
  later_event((sv_t *) v, time);
}

void update_button(sv_t *var)
{
  assert(var);
  assert(var->event_time == now);
  sv_button_t *v = (sv_button_t *) var;
  v->value = v->event_value;
}