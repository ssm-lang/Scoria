#ifndef PENG_LED_H
#define PENG_LED_H
#include <zephyr.h>

#include <stdbool.h>
#include <stdio.h>

#include <peng.h>

#include <ll/ll_driver.h>
#include <ll/ll_led.h>

typedef struct {
               /* Generic SV fields */
               void (*update)(sv_t *);
               struct trigger *triggers;
               peng_time_t last_updated;
               peng_time_t event_time;
               void (*to_string)(sv_t *, char *, size_t);

               /* LED specific fields */
               bool value;
               bool event_value;
               bool can_use_driver;
               ll_driver_t driver;
} sv_led_t;
void to_string_led(sv_t *v, char *buffer, size_t size);
void initialize_led(sv_led_t *v);
void initialize_ledIO(sv_led_t *v, uint32_t ledid);
void assign_led(sv_led_t *v, priority_t priority, bool value);
void later_led(sv_led_t *v, peng_time_t time, bool value);
void update_led(sv_t *var);
#endif // PENG_LED_H