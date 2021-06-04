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
#include <peng-led.h>
#include <peng-button.h>
#include <peng-program.h>

/* NOTE: Here we define the LEDs */
sv_led_t led0;
sv_led_t led1;
sv_led_t led2;
sv_led_t led3;
/* NOTE: Here we define the buttons*/
sv_button_t buttons[4];
sv_button_t *button0 = &buttons[0];
sv_button_t *button1 = &buttons[1];
sv_button_t *button2 = &buttons[2];
sv_button_t *button3 = &buttons[3];

void initialize_IO() {
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
  initialize_button(button0, 0, 0);
  button0->value = false;
  initialize_button(button1, 1, 1);
  button1->value = false;
  initialize_button(button2, 2, 2);
  button2->value = false;
  initialize_button(button3, 3, 3);
  button3->value = false;
}

void top_return(act_t *act) {
  return;
}

act_t top = { .step = top_return };

void initialize_cont() {
  /* Populate continuation queue */
  fork_routine( (act_t *) enter_flipled(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT) );
}
















/*
 * This is the message queue that will be used to interact with the RTS so
 * that events can be placed in the event queue and so on. When a deadline is
 * reached and the RTS should wake up to perform events, a message of type
 * 0 (look documentation of the `tick_message` struct) will be placed in the
 * queue.
 */
K_MSGQ_DEFINE(tick_msgq, sizeof(ll_driver_msg_t), 100,1);

/* Counter configuration - used to sleep until an event should happen */
struct  counter_alarm_cfg alarm_cfg;

/*
 * This thing will count ticks! The nrf52840dk does so at 16Mhz, but
 * Joel says it can be configured. There are functions to convert between
 * us and ticks, to reason about actual time. The SSM program above just
 * talks about ticks though (the delays are 0.25sec).
 */
const struct device *counter_dev = NULL;

/*
 * This is the tick thread, and it serves as the main loop of the program.
 * It blocks the program when we should wait for an event, and it handles
 * the delivery of events such as button presses to the RTS event queue.
 * It is constant.
 */
void tick_thread_main(void * a, void* b, void *c) {
  (void)a;
  (void)b;
  (void)c;

  /* Initialize IO peripherals */
  initialize_IO();
  /* Initialize RTS ready queue */
  initialize_cont();

  /* initialize model time */
  now = 0;

  ll_driver_msg_t recv_msg;
  while (1) {

    // might want two queues, tick queue and msg queue. Tick queue is 1 item per
    // time, and msg queue should be emptied before next invocation of tick
    /* get a data item, waiting as long as needed */
    k_msgq_get(&tick_msgq, &recv_msg, K_FOREVER);

    /* Handle the event accordingly */
    switch(recv_msg.msg_type) {
      case 0:
        break;
      case 23120: { // todo for now
        /* Only do things when we press a button down */
        /* Update now to reflect the time passed */
        uint32_t count;
        counter_get_value(counter_dev, &count);
        now = count;

        /* Fetch button variable from buttons schedule it in 1 tick */
        sv_button_t* button = &buttons[recv_msg.driver_id];
        later_button(button, now + 1, recv_msg.data);
        break;
      }
      default:
        printk("default case - type: %d\n", recv_msg.msg_type);
    }

    now = next_event_time();
    tick();

    peng_time_t next = next_event_time();

    if (next == ULLONG_MAX) {
      printk("NOTHING IN THE QUEUE\r\n");
      continue;
    }

    uint64_t wake_time = next_event_time(); /* Absolute time */

    alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
    alarm_cfg.ticks = wake_time;

    int r = counter_set_channel_alarm(counter_dev, 0, &alarm_cfg);
    if (!r) {
    } else {
      if (r == - ENOTSUP ) {
        printk("hw_tick: Error setting alarm (ENOTSUP)\r\n");
      } else if ( r == - EINVAL ) {
        printk("hw_tick: Error setting alarm (EINVAL)\r\n");
      } else if ( r == - ETIME ) {
        printk("hw_tick: Error setting alarm (ETIME)\r\n");
      } else {
        printk("hw_tick: Error setting alarm\r\n");
      }
    }
  }
}


/*
 * Two extra threads will be needed! One to react to button clicks, and
 * one to handle the actual RTS loop (tick, sleep, apply events, repeat).
 */
K_THREAD_STACK_DEFINE(tick_thread_stack, 512);
struct k_thread tick_thread;

/*
 * This function launches the thread that ticks the RTS when it should
 * wake up and perform events.
 */
void start_tick_thread(void) {
  k_thread_create(&tick_thread, tick_thread_stack,
		  K_THREAD_STACK_SIZEOF(tick_thread_stack),
		  tick_thread_main,
		  NULL, NULL, NULL,
		  5, 0, K_NO_WAIT);
}

/*
 * After an instant, the RTS will inspect the next event time and schedule
 * `counter_dev` to wake up this function at that time. This function will
 * then just package up a simple message and pass it along to the tick
 * queue the RTS uses to synchronise.
 */
void hw_tick(const struct device *dev, uint8_t chan, uint32_t ticks, void *user_data) {

  ll_driver_msg_t msg = { .driver_id = -1
                        , .msg_type  = 1
                        , .data      = 0
                        , .timestamp = 0
                        };
  k_msgq_put(&tick_msgq, &msg, K_NO_WAIT);
  /* this can fail to send the message. then what? */
}

/*
 * This is the main function, and it is constant. 
 */
void main(void) {

  printk("Sleeping 1 seconds\r\n");
  k_sleep(K_SECONDS(1)); // Wait enough for starting up a terminal.
  printk("WOKE UP\r\n");

  /* ************************* */
  /* Hardware timer experiment */
  counter_dev = device_get_binding(DT_LABEL(DT_ALIAS(ssm_timer)));
  if (!counter_dev) {
    printk("HWCounter: Device not found error\r\n");
  }

  if (counter_get_frequency(counter_dev) > 1000000) {
    printk("HWCounter: Running at %dMHz\r\n", counter_get_frequency(counter_dev) / 1000000);
  } else {
    printk("HWCounter: Running at %dHz\r\n", counter_get_frequency(counter_dev));
  }

  alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
  alarm_cfg.ticks = 10;
  alarm_cfg.callback = hw_tick;
  alarm_cfg.user_data = &alarm_cfg;


  if (!counter_set_channel_alarm(counter_dev, 0, &alarm_cfg)) {
    printk("HWCounter: Alarm set\r\n");
  } else {
    printk("HWCounter: Error setting alarm\r\n");
  }
  if (!counter_set_guard_period(counter_dev, UINT_MAX/2, COUNTER_GUARD_PERIOD_LATE_TO_SET)) {
    printk("HWCounter: Guard period set\r\n");
  } else {
    printk("HWCounter: Error setting guard period\r\n");
  }

  counter_start(counter_dev);
  printk("Starting Tick-Thread\r\n");
  start_tick_thread();
}