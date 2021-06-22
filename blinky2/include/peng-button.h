#ifndef PENG_BUTTON_
#define PENG_BUTTON_

#include <zephyr.h>

#include <stdbool.h>
#include <stdio.h>

#include <peng.h>

#include <ll/ll_driver.h>
#include <ll/ll_button.h>

typedef struct {
               /* Generic SV fields */
               void (*update)(sv_t *);
               struct trigger *triggers;
               peng_time_t last_updated;
               peng_time_t event_time;
               void (*to_string)(sv_t *, char *, size_t);

               /* Button specific fields */
               bool value;
               bool event_value;
               ll_driver_t driver;
             } sv_button_t;
void to_string_button(sv_t *v, char *buffer, size_t size);
void initialize_button(sv_button_t *v);
void initialize_buttonIO(sv_button_t *v, uint32_t driverid, uint32_t buttonid);
void assign_button(sv_button_t *v, priority_t priority, bool value);
void later_button(sv_button_t *v, peng_time_t time, bool value);
void update_button(sv_t *var);
#endif // PENG_BUTTON_H