
#include <zephyr.h>

#include <stdbool.h>
#include <stdio.h>

#include <peng.h>

#include <ll/ll_driver.h>
#include <ll/ll_led.h>

#include <peng-led.h>

/********** This code will be generated when LED IO is required **********/
void to_string_led(sv_t *v, char *buffer, size_t size) {
               sv_led_t* iv = (sv_led_t *) v;
               char str[] = "led %s";
               snprintf(buffer, size, str, iv->value ? "on" : "off");
               return;
}
void initialize_led(sv_led_t *v) {
               assert(v);
               /* Generic initialization */
               *v = (sv_led_t) { .to_string        = to_string_led
                                     , .update         = update_led
                                     , .triggers       = NULL
                                     , .last_updated   = now
                                     , .event_time     = NO_EVENT_SCHEDULED
                                     , .can_use_driver = false
                                     };
}
void initialize_ledIO(sv_led_t *v, uint32_t ledid) {
               assert(v);
               /* Generic initialization */
               *v = (sv_led_t) { .to_string        = to_string_led
                                     , .update         = update_led
                                     , .triggers       = NULL
                                     , .last_updated   = now
                                     , .event_time     = NO_EVENT_SCHEDULED
                                     , .can_use_driver = true
                                     };
               /* LED specific initialization */
               if(ll_led_init(&v->driver, ledid, 0)) {
                 printk("LED-driver %d successfully initialized\n", ledid);
               } else {
                 printk("LED-driver %d failed to initialize\n", ledid);
               }
}
void assign_led(sv_led_t *v, priority_t priority, bool value) {
               v->value = value;
               v->last_updated = now;
               schedule_sensitive((sv_t *) v, priority);
               
               /* After performing the assignment, reflect the new value in the driver */
               if(v->can_use_driver) {
                 uint8_t cmd = v->value ? 1 : 0;
                 ll_write(&v->driver, &cmd, 1);
               }
}
void later_led(sv_led_t *v, peng_time_t time, bool value) {
               assert(v);
               v->event_value = value;
               later_event((sv_t *) v, time);
}
void update_led(sv_t *var) {
               assert(var);
               assert(var->event_time == now);
               sv_led_t *v = (sv_led_t *) var;
               v->value = v->event_value;

               /* After updating the value, reflect the new value in the driver */
               if(v->can_use_driver) {
                 uint8_t cmd = v->value ? 1 : 0;
                 ll_write(&v->driver, &cmd, 1);
               }
}

/********** End of LED IO file **********/