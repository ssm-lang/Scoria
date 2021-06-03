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

/********** Some structs and externs which will be needed later **********/

extern peng_time_t now;

/*
 * Struct representing the information that is relevant when a button
 * has been pressed. The `driverid` will correspond to an index in the
 * `buttons` array, which we can use to look up which sv_button_t var
 * should be updated. The button event is either 0 or 1, representing button
 * released and button pressed.
 */
typedef struct {
  uint8_t driverid;
  uint8_t buttonevent;
} button_msg;

/*
 * Before each invocation of `tick`, a message of this type will be handled
 * by the RTS loop.
 * Message types:
 *   0: Deadline reached, need to wake up and perform scheduled events
 *   1: Button was pressed, need to populate event queue 
 */
struct tick_message {
  uint8_t type;
  union {
    bool nothing;
    button_msg button_data;
  } data;
};

#define MAX_MESSAGES  100
#define MSG_ALIGNMENT 1

/*
 * This is the message queue that will be used to interact with the RTS so
 * that events can be placed in the event queue and so on. When a deadline is
 * reached and the RTS should wake up to perform events, a message of type
 * 0 (look documentation of the `tick_message` struct) will be placed in the
 * queue.
 */
K_MSGQ_DEFINE(tick_msgq, sizeof(struct tick_message), MAX_MESSAGES,MSG_ALIGNMENT);

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
sv_button_t buttons[4];

void to_string_button(sv_t *v, char *buffer, size_t size);
void initialize_button(sv_button_t *v, uint32_t driverid, uint32_t buttonid); // NOTE: Extra fields here
void assign_button(sv_button_t *v,  priority_t priority, bool value);
void later_button(sv_button_t *v, peng_time_t time, bool value);
void update_button(sv_t *var);

/*
 * The buttons will need a buffer and a queue to communicate with our thread.
 * When a button is pressed a message of type `ll_driver_msg_t` will be placed
 * in the below defined message queue. The message contains information of which
 * button was pushed and what button event was produced (up/down).
 */
char buttons_buffer[10*sizeof(ll_driver_msg_t)];
struct k_msgq button_queue;

/* This is part of the ll-driver interface Joel & Abi wrote */
extern int send_message(zephyr_interop_t* this, ll_driver_msg_t msg);
zephyr_interop_t button_interop = { .msgq         = &button_queue
                                  , .send_message = send_message
                                  };

void init_button_interop() {
  k_msgq_init(&button_queue, buttons_buffer, sizeof(ll_driver_msg_t), 10);
}

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

/*
 * When a button is pressed, as we said earlier, a message will be placed in
 * a queue. This function is meant to be run in its own thread, and it will
 * continuously wait for messages to appear in this queue, and then it does, it
 * will repack them and place them in the tick queue that the runtime system
 * uses to deal with wakeups etc.
 */
void button_thread_loop() {
  ll_driver_msg_t btn_msg;
  struct tick_message tick_msg;
  while(1) {
    k_msgq_get(&button_queue, &btn_msg, K_FOREVER);
    tick_msg.type = 1;
    button_msg button_data = { .driverid    = btn_msg.driver_id
                             , .buttonevent = btn_msg.data
                             };
    tick_msg.data.button_data = button_data;
    k_msgq_put(&tick_msgq, &tick_msg, K_NO_WAIT);
  }
}

/********** End of button code **********/

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
  trigger_t trig2;
  trigger_t trig3;
  trigger_t trig4;
} act_flipled_t;

act_flipled_t *enter_flipled(act_t *caller, priority_t priority, depth_t depth);
void step_flipled(act_t *gen_act);

act_flipled_t *enter_flipled(act_t *caller, priority_t priority, depth_t depth)
{
    act_t *gen_act = enter(sizeof(act_flipled_t), step_flipled, caller, priority, depth);
    act_flipled_t *act = (act_flipled_t *) gen_act;
    act->trig1.act = (act_t *) act;
    act->trig2.act = (act_t *) act;
    act->trig3.act = (act_t *) act;
    act->trig4.act = (act_t *) act;
    return act;
}

/*
 * When the programmer wrote this procedure in our EDSL he or she used e.g
 * `getLed 0`, `getLed 1` etc, and those calls resulted in (apart from all
 * the fluff above) the below variables to be declared. They will be declared
 * for real and initialized further down, in the main function.
 * 
 * The LED etc does not appear as a parameter to the function, so it might be
 * tricky/might not make sense to model it as such in the generated code. The
 * LED would need to trickle down through the entire callstack, maybe passing
 * by as an argument to a procedure that will not need it. A global variable
 * seems more appropriate.
 */
extern sv_led_t led0;
extern sv_led_t led1;
extern sv_led_t led2;
extern sv_led_t led3;
extern sv_button_t *button0;
extern sv_button_t *button1;
extern sv_button_t *button2;
extern sv_button_t *button3;

void step_flipled(act_t *gen_act)
{
    act_flipled_t *act = (act_flipled_t *) gen_act;
    
    switch(act->pc) {
        case 0:
            // while
            L0:
            if (!(true)) goto L1;
            sensitize((sv_t *)button0, &act->trig1);
            act->pc = 1;
            return;
        case 1:
            desensitize(&act->trig1);
            later_led(&led0, now + 4000000, true);
            sensitize((sv_t *)&led0, &act->trig1);
            act->pc = 2;
            return;
        case 2:
            desensitize(&act->trig1);
            later_led(&led0, now + 4000000, false);
            sensitize((sv_t *)&led0, &act->trig1);
            act->pc = 3;
            return;
        case 3:
            desensitize(&act->trig1);
            later_led(&led0, now + 4000000, true);
            sensitize((sv_t*)&led0, &act->trig1);
            act->pc = 4;
            return;
        case 4:
            desensitize(&act->trig1);
            later_led(&led0, now + 4000000, false);
            sensitize((sv_t*)&led0, &act->trig1);
            act->pc = 5;
            return;
        case 5:
            desensitize(&act->trig1);
            later_led(&led0, now + 4000000, true);
            sensitize((sv_t*)&led0, &act->trig1);
            act->pc = 6;
            return;
        case 6:
            desensitize(&act->trig1);
            later_led(&led0, now + 4000000, false);
            sensitize((sv_t*)&led0, &act->trig1);
            act->pc = 7;
            return;
        case 7:
            desensitize(&act->trig1);
            later_led(&led0, now + 4000000, true);
            sensitize((sv_t*)&led0, &act->trig1);
            act->pc = 8;
            return;
        case 8:
            desensitize(&act->trig1);
            later_led(&led0, now + 4000000, false);
            sensitize((sv_t*)&led0, &act->trig1);
            goto L0;
            L1:
        case 9:
        leave((act_t *) act, sizeof(act_flipled_t));
    }

    // switch(act->pc) {
    //     case 0:
    //         // while
    //         L0:
    //         if (!(true)) goto L1;
    //         sensitize((sv_t *)button0, &act->trig1);
    //         sensitize((sv_t *)button1, &act->trig2);
    //         sensitize((sv_t *)button2, &act->trig3);
    //         sensitize((sv_t *)button3, &act->trig4);
    //         act->pc = 1;
    //         return;
    //     case 1:
    //         desensitize(&act->trig1);
    //         desensitize(&act->trig2);
    //         desensitize(&act->trig3);
    //         desensitize(&act->trig4);
    //         if(event_on((sv_t*)button0)) {
    //           assign_led(&led0, act->priority, led0.value ? false : true);
    //         }
    //         if(event_on((sv_t*)button1)) {
    //           assign_led(&led1, act->priority, led1.value ? false : true);
    //         }
    //         if(event_on((sv_t*)button2)) {
    //           assign_led(&led2, act->priority, led2.value ? false : true);
    //         }
    //         if(event_on((sv_t*)button3)) {
    //           assign_led(&led3, act->priority, led3.value ? false : true);
    //         }
    //         goto L0;
    //         L1:
    //     case 3:
    //     leave((act_t *) act, sizeof(act_flipled_t));
    // }
}

/********** End of SSM program **********/

#define PRINT printk

/* ********** */
/*   TIMER    */
#define TIMER DT_LABEL(DT_ALIAS(ssm_timer))

/* Counter configuration - used to sleep until an event should happen */
struct  counter_alarm_cfg alarm_cfg;

/*
 * This thing will count ticks! The nrf52840dk does so at 16Mhz, but
 * Joel says it can be configured. There are functions to convert between
 * us and ticks, to reason about actual time. The SSM program above just
 * talks about ticks though (the delays are 0.25sec).
 */
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

/*
 * After an instant, the RTS will inspect the next event time and schedule
 * `counter_dev` to wake up this function at that time. This function will
 * then just package up a simple message and pass it along to the tick
 * queue the RTS uses to synchronise.
 */
void hw_tick(const struct device *dev, uint8_t chan, uint32_t ticks, void *user_data) {

  /* Put a tick on the messagebox */
  struct tick_message msg = { .type         = 0
                            , .data.nothing = 0
                            };
  k_msgq_put(&tick_msgq, &msg, K_NO_WAIT);
  /* this can fail to send the message. then what? */
}

/*
 * Two extra threads will be needed! One to react to button clicks, and
 * one to handle the actual RTS loop (tick, sleep, apply events, repeat).
 */
K_THREAD_STACK_DEFINE(tick_thread_stack, 512);
struct k_thread tick_thread;

K_THREAD_STACK_DEFINE(button_thread_stack, 512);
struct k_thread button_thread;

/* NOTE: Here we define the LEDs */
sv_led_t led0;
sv_led_t led1;
sv_led_t led2;
sv_led_t led3;
/* NOTE: Here we define the buttons*/
sv_button_t *button0;
sv_button_t *button1;
sv_button_t *button2;
sv_button_t *button3;

void tick_thread_main(void * a, void* b, void *c) {
  (void)a;
  (void)b;
  (void)c;

  /* NOTE: Here the LEDs is initialized*/
  initialize_led(&led0, 0);
  led0.value = false;
  initialize_led(&led1, 1);
  led1.value = false;
  initialize_led(&led2, 2);
  led2.value = false;
  initialize_led(&led3, 3);
  led3.value = false;

  /* NOTE: Here the Buttons are initialized*/
  init_button_interop();
  button0 = &buttons[0];
  initialize_button(button0, 0, 0);
  button0->value = false;
  button1 = &buttons[1];
  initialize_button(button1, 1, 1);
  button1->value = false;
  button2 = &buttons[2];
  initialize_button(button2, 2, 2);
  button2->value = false;
  button3 = &buttons[3];
  initialize_button(button3, 3, 3);
  button3->value = false;

  /* initialize model time */
  now = 0;

  /* Populate continuation queue */
  fork_routine( (act_t *) enter_flipled(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT) );

  struct tick_message recv_msg;
  while (1) {

    /* get a data item, waiting as long as needed */
    k_msgq_get(&tick_msgq, &recv_msg, K_FOREVER);

    /* Handle the event accordingly */
    switch(recv_msg.type) {
      case 0:
        break;
      case 1:
        /* Only do things when we press a button down */
        if(recv_msg.data.button_data.buttonevent == 1) {
          /* Update now to reflect the time passed */
          uint32_t count;
          counter_get_value(counter_dev, &count);
          now = count;

          /* Fetch button event from queue and schedule it in 1 tick */
          button_msg bd = recv_msg.data.button_data;
          sv_button_t* button = &buttons[bd.driverid];
          later_button(button, now + 1, bd.buttonevent);
          continue;
        }   
    }

    now = next_event_time();
    tick();

    peng_time_t next = next_event_time();

    if (next == ULLONG_MAX) {
      /* This just means that there are no events in the queue (or a remarkable coincidence) */
      /* What to do in this case ?*/
      /*  - Go to sleep and await being woken from outside source */

      PRINT("NOTHING IN THE QUEUE\r\n");
      continue;
    }

    uint64_t wake_time = next_event_time(); /* Absolute time */

    alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
    alarm_cfg.ticks = wake_time;

    int r = counter_set_channel_alarm(counter_dev, 0, &alarm_cfg);
    if (!r) {
    } else {
      if (r == - ENOTSUP ) {
        PRINT("hw_tick: Error setting alarm (ENOTSUP)\r\n");
      } else if ( r == - EINVAL ) {
        PRINT("hw_tick: Error setting alarm (EINVAL)\r\n");
      } else if ( r == - ETIME ) {
        PRINT("hw_tick: Error setting alarm (ETIME)\r\n");
      } else {
        PRINT("hw_tick: Error setting alarm\r\n");
      }
    }
  }
}

void start_tick_thread(void) {

  k_thread_create(&tick_thread, tick_thread_stack,
		  K_THREAD_STACK_SIZEOF(tick_thread_stack),
		  tick_thread_main,
		  NULL, NULL, NULL,
		  5, 0, K_NO_WAIT);
  
  k_thread_create(&button_thread, button_thread_stack,
      K_THREAD_STACK_SIZEOF(tick_thread_stack),
      button_thread_loop,
      NULL, NULL, NULL,
      5, 0, K_NO_WAIT);
}

/* ************************************************************ */
/* MAIN                                                         */
/* ************************************************************ */

void main(void) {

  PRINT("Sleeping 1 seconds\r\n");
  k_sleep(K_SECONDS(1)); // Wait enough for starting up a terminal.
  PRINT("WOKE UP\r\n");

  /* ************************* */
  /* Hardware timer experiment */
  counter_dev = device_get_binding(TIMER);
  if (!counter_dev) {
    PRINT("HWCounter: Device not found error\r\n");
  }

  if (counter_get_frequency(counter_dev) > 1000000) {
    PRINT("HWCounter: Running at %dMHz\r\n", counter_get_frequency(counter_dev) / 1000000);
  } else {
    PRINT("HWCounter: Running at %dHz\r\n", counter_get_frequency(counter_dev));
  }

  alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
  alarm_cfg.ticks = 10;
  alarm_cfg.callback = hw_tick;
  alarm_cfg.user_data = &alarm_cfg;


  if (!counter_set_channel_alarm(counter_dev, 0, &alarm_cfg)) {
    PRINT("HWCounter: Alarm set\r\n");
  } else {
    PRINT("HWCounter: Error setting alarm\r\n");
  }
  if (!counter_set_guard_period(counter_dev, UINT_MAX/2, COUNTER_GUARD_PERIOD_LATE_TO_SET)) {
    PRINT("HWCounter: Guard period set\r\n");
  } else {
    PRINT("HWCounter: Error setting guard period\r\n");
  }

  counter_start(counter_dev);
  PRINT("Starting Tick-Thread\r\n");
  start_tick_thread();
}

/* Function used by the ll-driver to deliver button events */
int send_message(zephyr_interop_t* this, ll_driver_msg_t msg) {
   return k_msgq_put(this->msgq,(void*)&msg, K_NO_WAIT);
}