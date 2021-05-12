#include <zephyr/types.h>
#include <stddef.h>
#include <stdio.h>
#include <errno.h>
#include <zephyr.h>
#include <sys/printk.h>
#include <sys/byteorder.h>
#include <sys/ring_buffer.h>

#include <drivers/gpio.h>
#include <drivers/uart.h>
#include <drivers/i2c.h>
#include <drivers/sensor.h>
#include <drivers/counter.h>

#include "fib.h"
#include "blinky.h"

#include "usb_cdc.h"
#include "led.h"

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

sv_int_t r;

void tick_thread_main(void * a, void* b, void *c) {
  (void)a;
  (void)b;
  (void)c;

  //sv_int_t r;
  initialize_int(&r);
  r.value = 0;
  now = 0;
  sv_int_t led;
  initialize_int(&led);
  led.value = 0;

  uint8_t recv_msg;

  //PRINT("forking myfib\r\n");
  //fork_routine( (act_t *) enter_myfib(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT, 4, &r) );

  PRINT("forking blinky\r\n");
  fork_routine( (act_t *) enter_main(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT, &led) );

  PRINT("Waiting for ticks\r\n");
  int i = 0;
  while (1) {

    /* get a data item, waiting as long as needed */
    k_msgq_get(&tick_msgq, &recv_msg, K_FOREVER);

    uint32_t count;
    counter_get_value(counter_dev, &count);


    PRINT("*** *** *** *** *** ***\r\n");
    PRINT("ctr: %u\r\n", count);
    PRINT("now: %llu\r\n", now);
    PRINT("next: %llu\r\n", next_event_time());

    now = next_event_time();
    tick();

    uint64_t next = next_event_time();
    PRINT("next after tick: %llu\r\n", next);

    if (next == ULLONG_MAX) {
      /* This just means that there are no events in the queue (or a remarkable coincidence) */
      /* What to do in this case ?*/
      /*  - Go to sleep and await being woken from outside source */

      PRINT("NOTHING IN THE QUEUE\r\n");
    }

    uint64_t wake_time = next_event_time(); /* Absolute time */

    //PRINT("sleep_time = %lld\r\n", sleep_time);

    alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
    alarm_cfg.ticks = wake_time;

    int r = counter_set_channel_alarm(counter_dev, 0, &alarm_cfg);
    if (!r) {
      //PRINT("hw_tick: Alarm set\r\n");
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

    i++;
  }
}

void start_tick_thread(void) {

  k_thread_create(&tick_thread, tick_thread_stack,
		  K_THREAD_STACK_SIZEOF(tick_thread_stack),
		  tick_thread_main,
		  NULL, NULL, NULL,
		  5, 0, K_NO_WAIT);
}

/* ************************************************************ */
/* MAIN                                                         */
/* ************************************************************ */

void main(void) {

  /* *************  */
  /* Start USB_CDC  */
  //  start_usb_cdc_thread();

  //PRINT("USB_CDC: started\r\n");
  PRINT("Sleeping 1 seconds\r\n");
  k_sleep(K_SECONDS(1)); // Wait enough for starting up a terminal.
  PRINT("WOKE UP\r\n");

  PRINT("Init LEDs\r\n");
  led_driver_t *led0 = led_init(0);
  if (!led0) {
    PRINT("ERROR INIT LEDs\r\n");
    return;
  }

  PRINT("Init Blinky\r\n");
  init_blinky();

  /* ************************* */
  /* Hardware timer experiment */
  PRINT("Configuring hardware timer \r\n");
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
  alarm_cfg.ticks = 10; //counter_us_to_ticks(counter_dev, 0);
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

  /* configure uart */

  PRINT("Starting Tick-Thread\r\n");
  start_tick_thread();

  int led0_state = 0;

  while(1) {
    led_set(led0,led0_state);
    led0_state = 1 - led0_state;
    k_sleep(K_SECONDS(4));
  }
}
