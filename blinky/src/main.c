/*
 * Copyright (c) 2016 Intel Corporation
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#include <zephyr.h>
#include <drivers/counter.h>
#include <zephyr/types.h>
#include <stddef.h>

#include <ll/ll_driver.h>
#include <ll/ll_led.h>
#include <ll/ll_button.h>
#include <hal/zephyr/svm_zephyr.h>

#include <stdbool.h>
#include <stdio.h>

#include <peng.h>

/********** This code will be generated when LED IO is required **********/

extern peng_time_t now;

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
  ll_driver_t driver;
  bool value;
  bool event_value;
} sv_led_t;

void to_string_led(sv_t *v, char *buffer, size_t size);
void initialize_led(sv_led_t *v, uint32_t ledid); // NOTE: Extra field here
void assign_led(sv_led_t *v,  priority_t priority, bool value);
void later_led(sv_led_t *v, peng_time_t time, bool value);
void update_led(sv_t *var);

void to_string_led(sv_t *v, char *buffer, size_t size) {
    sv_led_t* iv = (sv_led_t *) v;
    char str[] = "led %s";
    snprintf(buffer, size, str, iv->value ? "on" : "off");
}

void initialize_led(sv_led_t *v, uint32_t ledid) {
  assert(v);
  /* Generic initialization */
  *v = (sv_led_t) { .to_string    = to_string_led,
                    .update       = update_led,
                    .triggers     = NULL,
			        .last_updated = now,
			        .event_time   = NO_EVENT_SCHEDULED };

  /* LED specific initialization */
  if(ll_led_init(&v->driver, ledid, 0)) {
      printk("LED-driver %d successfully initialized\n", ledid);
  } else {
      printk("driver failed to initialize\n");
  }
}

void assign_led(sv_led_t *v,  priority_t priority, bool value) {
  v->value = value;
  v->last_updated = now;
  schedule_sensitive((sv_t *) v, priority);

  /* After performing the assignment, reflect the new value in the driver */
  uint8_t cmd = v->value ? 1 : 0;
  ll_write(&v->driver, &cmd, 1);
}

void later_led(sv_led_t *v, peng_time_t time, bool value) {
  assert(v);
  v->event_value = value;
  later_event((sv_t *) v, time);
}

void update_led(sv_t *var)
{
  assert(var);
  assert(var->event_time == now);
  sv_led_t *v = (sv_led_t *) var;
  v->value = v->event_value;

  /* After updating the value, reflect the new value in the driver */
  uint8_t cmd = v->value ? 1 : 0;
  ll_write(&v->driver, &cmd, 1);
}

/********** End of LED IO file **********/

/********** SSM program **********/

typedef struct {
  stepf_t *step;
  struct act *caller;
  uint16_t pc;
  uint16_t children;
  priority_t priority;
  depth_t depth;
  bool scheduled;
  trigger_t trig1;
} act_flipled_t;

act_flipled_t *enter_flipled(act_t *caller, priority_t priority, depth_t depth);
void step_flipled(act_t *gen_act);

act_flipled_t *enter_flipled(act_t *caller, priority_t priority, depth_t depth)
{
    act_t *gen_act = enter(sizeof(act_flipled_t), step_flipled, caller, priority, depth);
    act_flipled_t *act = (act_flipled_t *) gen_act;
    act->trig1.act = (act_t *) act;
    return act;
}

extern sv_led_t led0;

/* The idea is that when this procedure was written in the EDSL, the programmer
 * would get access to the LED by calling e.g `getLed 0`. When this call is made all
 * the above crap is scheduled for generation during compilation, and this procedure
 * gets access to the LED through the variable above.
 */
void step_flipled(act_t *gen_act)
{
    act_flipled_t *act = (act_flipled_t *) gen_act;
    
    switch(act->pc) {
        case 0:
            // while
            L0:
            if (!(true)) goto L1;
            later_led(&led0, now + 8000000UL, true);
            // if this was e.g a button, only enable the button
            // callback when someone waits for the button.
            // Disable it when no one is waiting for it.
            sensitize((sv_t *)&led0, &act->trig1);
            act->pc = 1;
            return;
        case 1:
            desensitize(&act->trig1);
            later_led(&led0, now + 8000000UL, false);
            sensitize((sv_t *)&led0, &act->trig1);
            act->pc = 2;
            return;
        case 2:
            desensitize(&act->trig1);
            goto L0;
            L1:
        case 3:
        leave((act_t *) act, sizeof(act_flipled_t));
    }
}

/********** End of SSM program **********/













































/* Below follows lots of Joel-magic I don't quite understand~ */

//#define PRINT usb_printf
#define PRINT printk

/* ********** */
/*   TIMER    */

#define TIMER DT_LABEL(DT_ALIAS(ssm_timer))
/* Not sure how to do the similar thing on the STM32 yet. */

struct  counter_alarm_cfg alarm_cfg;

const struct device *counter_dev = NULL;

/* *************** */
/*   TOP           */

void top_return(act_t *act)
{
  return;
}

act_t top = { .step = top_return };

/* ************************************************************ */
/* hw_tick                                                      */
/* ************************************************************ */

#define MAX_MESSAGES  100
#define MSG_ALIGNMENT 1


K_MSGQ_DEFINE(tick_msgq, 1, MAX_MESSAGES,MSG_ALIGNMENT);

void hw_tick(const struct device *dev, uint8_t chan, uint32_t ticks, void *user_data) {

  /* Put a tick on the messagebox */
  uint8_t msg = 1;
  k_msgq_put(&tick_msgq, &msg, K_NO_WAIT);
  /* this can fail to send the message. then what? */
}

K_THREAD_STACK_DEFINE(tick_thread_stack, 512);
struct k_thread tick_thread;

/* NOTE: Here we define the LED */
sv_led_t led0;

// void tick_thread_main(void * a, void* b, void *c) {
//   (void)a;
//   (void)b;
//   (void)c;

//   /* NOTE: Here the LED is initialized*/
//   initialize_led(&led0, 0);
//   led0.value = false;

//   now = 0;

//   uint8_t recv_msg;

//   fork_routine( (act_t *) enter_flipled(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT) );

//   int i = 0;
//   while (1) {

//     /* get a data item, waiting as long as needed */
//     k_msgq_get(&tick_msgq, &recv_msg, K_FOREVER);

//     now = next_event_time();
//     tick();

//     uint64_t next = next_event_time();

//     if (next == ULLONG_MAX) {
//       /* This just means that there are no events in the queue (or a remarkable coincidence) */
//       /* What to do in this case ?*/
//       /*  - Go to sleep and await being woken from outside source */

//       PRINT("NOTHING IN THE QUEUE\r\n");
//     }

//     uint64_t wake_time = next_event_time(); /* Absolute time */

//     //PRINT("sleep_time = %lld\r\n", sleep_time);

//     alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
//     alarm_cfg.ticks = wake_time;

//     int r = counter_set_channel_alarm(counter_dev, 0, &alarm_cfg);
//     if (!r) {
//       //PRINT("hw_tick: Alarm set\r\n");
//     } else {
//       if (r == - ENOTSUP ) {
// 	PRINT("hw_tick: Error setting alarm (ENOTSUP)\r\n");
//       } else if ( r == - EINVAL ) {
// 	PRINT("hw_tick: Error setting alarm (EINVAL)\r\n");
//       } else if ( r == - ETIME ) {
// 	PRINT("hw_tick: Error setting alarm (ETIME)\r\n");
//       } else {
// 	PRINT("hw_tick: Error setting alarm\r\n");
//       }
//     }

//     i++;
//   }
// }

// void start_tick_thread(void) {

//   k_thread_create(&tick_thread, tick_thread_stack,
// 		  K_THREAD_STACK_SIZEOF(tick_thread_stack),
// 		  tick_thread_main,
// 		  NULL, NULL, NULL,
// 		  5, 0, K_NO_WAIT);
// }

// /* ************************************************************ */
// /* MAIN                                                         */
// /* ************************************************************ */


// void main(void) {

//   PRINT("Sleeping 1 seconds\r\n");
//   k_sleep(K_SECONDS(1)); // Wait enough for starting up a terminal.
//   PRINT("WOKE UP\r\n");

//   /* ************************* */
//   /* Hardware timer experiment */
//   counter_dev = device_get_binding(TIMER);
//   if (!counter_dev) {
//     PRINT("HWCounter: Device not found error\r\n");
//   }

//   if (counter_get_frequency(counter_dev) > 1000000) {
//     PRINT("HWCounter: Running at %dMHz\r\n", counter_get_frequency(counter_dev) / 1000000);
//   } else {
//     PRINT("HWCounter: Running at %dHz\r\n", counter_get_frequency(counter_dev));
//   } // Här kanske jag kan räkna ut hur många ticks som är en ms

//   alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
//   alarm_cfg.ticks = 10; //counter_us_to_ticks(counter_dev, 0);
//   alarm_cfg.callback = hw_tick;
//   alarm_cfg.user_data = &alarm_cfg;


//   if (!counter_set_channel_alarm(counter_dev, 0, &alarm_cfg)) {
//     PRINT("HWCounter: Alarm set\r\n");
//   } else {
//     PRINT("HWCounter: Error setting alarm\r\n");
//   }
//   if (!counter_set_guard_period(counter_dev, UINT_MAX/2, COUNTER_GUARD_PERIOD_LATE_TO_SET)) {
//     PRINT("HWCounter: Guard period set\r\n");
//   } else {
//     PRINT("HWCounter: Error setting guard period\r\n");
//   }

//   counter_start(counter_dev);

//   /* configure uart */

//   PRINT("Starting Tick-Thread\r\n");
//   start_tick_thread();
// }


int send_message(zephyr_interop_t* this, ll_driver_msg_t msg) {
  return k_msgq_put(this->msgq,(void*)&msg, K_NO_WAIT);
}

void main(void) {
  char buffer[10 * sizeof(ll_driver_msg_t)];
  struct k_msgq q;
  k_msgq_init(&q, buffer, sizeof(ll_driver_msg_t), 10);

  zephyr_interop_t t;
  t.msgq         = &q;
  t.send_message = send_message;

  ll_driver_t button;
  ll_button_init(&button, 0, &t, 0);

  while(1) {
    ll_driver_msg_t msg;
    k_msgq_get(&q, &msg, K_FOREVER);
    printk("felt click, drive value: %u\n", msg.data);
  }
}