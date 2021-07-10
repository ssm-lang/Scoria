/*
 * Copyright (c) 2016 Intel Corporation
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#include "ssm-program.h"
#include <device.h>
#include <devicetree.h>
#include <drivers/gpio.h>
#include <zephyr.h>

/* 1000 msec = 1 sec */
#define SLEEP_TIME_MS 1000

/* The devicetree node identifier for the "led0" alias. */
#define LED0_NODE DT_ALIAS(led0)

#if DT_NODE_HAS_STATUS(LED0_NODE, okay)
#define LED0 DT_GPIO_LABEL(LED0_NODE, gpios)
#define PIN DT_GPIO_PIN(LED0_NODE, gpios)
#define FLAGS DT_GPIO_FLAGS(LED0_NODE, gpios)
#else
/* A build error here means your board isn't set up to blink an LED. */
#error "Unsupported board: led0 devicetree alias is not defined"
#define LED0 ""
#define PIN 0
#define FLAGS 0
#endif

typedef struct {
  struct act act;
  u64_svt *ref2;
  struct trigger trig1;
} act_fun1_t;

extern struct act *enter_fun1(struct act *caller, priority_t priority,
                              depth_t depth, u64_svt *ref2);
extern void step_fun1(struct act *actg);

extern void top_return(struct act *act);

const struct device *dev;
void (*old_update)(struct sv *);

void output_update(struct sv *sv) {
  old_update(sv);
  u64_svt *v = container_of(sv, u64_svt, sv);
    gpio_pin_set(dev, PIN, 1);
    k_msleep(100);
    gpio_pin_set(dev, PIN, 0);
    k_msleep(100);
    gpio_pin_set(dev, PIN, 1);
    k_msleep(100);
    gpio_pin_set(dev, PIN, 0);
    k_msleep(100);
  gpio_pin_set(dev, PIN, (int)v->value);
}

int main(void) {
  dev = device_get_binding(LED0);
  if (dev == NULL) {
    return 1;
  }

  if (gpio_pin_configure(dev, PIN, GPIO_OUTPUT_ACTIVE | FLAGS) < 0) {
    return 1;
  }
  gpio_pin_set(dev, PIN, 0);
  k_msleep(1000);
  gpio_pin_set(dev, PIN, 1);
  k_msleep(300);
  gpio_pin_set(dev, PIN, 0);
  k_msleep(300);
  gpio_pin_set(dev, PIN, 1);
  k_msleep(300);
  gpio_pin_set(dev, PIN, 0);
  k_msleep(300);
  gpio_pin_set(dev, PIN, 1);
  k_msleep(300);
  gpio_pin_set(dev, PIN, 0);
  k_msleep(1000);

  struct act top = {.step = top_return};
  u64_svt ref2;
  set_now(0);

  initialize_u64(&ref2);
  ref2.value = 0;
  old_update = ref2.sv.update;
  ref2.sv.update = output_update;

  DEBUG_SV_SET_VAR_NAME(ref2.sv.debug, "ref2");
  act_fork(enter_fun1(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT, &ref2));

  ssm_time_t next;

  do {
    tick();
    next = next_event_time();
    k_msleep(SLEEP_TIME_MS * (next - get_now()));
    set_now(next);

    gpio_pin_set(dev, PIN, 0);
    k_msleep(1000);
    gpio_pin_set(dev, PIN, 1);
    k_msleep(300);
    gpio_pin_set(dev, PIN, 0);
    k_msleep(300);
    gpio_pin_set(dev, PIN, 1);
    k_msleep(300);
    gpio_pin_set(dev, PIN, 0);
    k_msleep(1000);

  } while (next != NO_EVENT_SCHEDULED);

  gpio_pin_set(dev, PIN, 0);
  k_msleep(1000);
  gpio_pin_set(dev, PIN, 1);
  k_msleep(300);
  gpio_pin_set(dev, PIN, 0);
  k_msleep(300);
  gpio_pin_set(dev, PIN, 1);
  k_msleep(300);
  gpio_pin_set(dev, PIN, 0);
  k_msleep(300);
  gpio_pin_set(dev, PIN, 1);
  k_msleep(300);
  gpio_pin_set(dev, PIN, 0);
  k_msleep(300);
  gpio_pin_set(dev, PIN, 1);
  k_msleep(300);
  gpio_pin_set(dev, PIN, 0);
  k_msleep(1000);

  DEBUG_PRINT("result %s %s %s\n", DEBUG_SV_GET_VAR_NAME(ref2.sv.debug),
              DEBUG_SV_GET_TYPE_NAME(ref2.sv.debug),
              DEBUG_SV_GET_VALUE_REPR(ref2.sv.debug, &ref2.sv));
  return 0;
}
