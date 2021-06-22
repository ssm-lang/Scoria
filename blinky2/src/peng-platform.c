
#include <peng-led.h>
#include <peng-button.h>
#include <zephyr.h>
#include <drivers/counter.h>
#include <zephyr/types.h>
#include <stddef.h>
#include <ll/ll_driver.h>
#include <ll/ll_led.h>
#include <hal/zephyr/svm_zephyr.h>
#include <stdbool.h>
#include <stdio.h>
#include <peng.h>
#include <peng-platform.h>
#include <stdint.h>

extern void initializeIO();
extern void initializeCont();
extern void handle_msg(ll_driver_msg_t *msg);

const struct device *counter_dev = NULL;
void get_hw_now(uint64_t *time) {
  uint32_t count;
  counter_get_value(counter_dev, &count);
  *time = count;
}

void top_return(act_t *act)
{
    return;
}
act_t top = {.step =top_return};
K_MSGQ_DEFINE(tick_msgq, sizeof(ll_driver_msg_t), 100, 1);
struct counter_alarm_cfg alarm_cfg;
void tick_thread_main(void *a, void *b, void *c)
{
    (void) a;
    (void) b;
    (void) c;
    initializeIO();
    initializeCont();
    now = 0;
    
    ll_driver_msg_t recv_msg;
    
    while (1) {
        k_msgq_get(&tick_msgq, &recv_msg, K_FOREVER);
        handle_driver_msg(&recv_msg);

        now = next_event_time();
        tick();
        
        peng_time_t next = next_event_time();
        
        if (next == ULLONG_MAX) {
            printk("NOTHING IN TE QUEUE\r\n");
            continue;
        }
        
        uint64_t wake_time = next_event_time();
        
        /* Absolute time */
        alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
        alarm_cfg.ticks = wake_time;
        
        int r = counter_set_channel_alarm(counter_dev, 0, &alarm_cfg);
        
        if (!r) { } else if (r == -ENOTSUP)
            printk("hw_tick: Error setting alarm (ENOTSUP)\r\n");
        else if (r == -EINVAL)
            printk("hw_tick: Error setting alarm (EINVAL)\r\n");
        else if (r == -ETIME)
            printk("hw_tick: Error setting alarm (ETIME)\r\n");
        else
            printk("hw_tick: Error setting alarm\r\n");
    }
    return;
}
K_THREAD_STACK_DEFINE(tick_thread_stack, 512);
struct k_thread tick_thread;
void start_tick_thread(void)
{
    k_thread_create(&tick_thread, tick_thread_stack, K_THREAD_STACK_SIZEOF(tick_thread_stack), tick_thread_main, NULL,
                    NULL, NULL, 5, 0, K_NO_WAIT);
}
void hw_tick(const struct device *dev, uint8_t chan, uint32_t ticks, void *user_data)
{
    ll_driver_msg_t msg = {.driver_id =-1, .msg_type =0, .data =0, .timestamp =0};
    
    k_msgq_put(&tick_msgq, &msg, K_NO_WAIT);
}
void main(void)
{
    printk("Sleeping 1 seconds\r\n");
    k_sleep(K_SECONDS(1));
    printk("Woke up\r\n");
    counter_dev = device_get_binding(DT_LABEL(DT_ALIAS(ssm_timer)));
    if (!counter_dev)
        printk("HWCounter: Device not found error\r\n");
    if (counter_get_frequency(counter_dev) > 1000000)
        printk("HWCounter: Running at %dMHz\r\n", counter_get_frequency(counter_dev) / 1000000);
    else
        printk("HWCounter: Running at %dHz\r\n", counter_get_frequency(counter_dev));
    alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
    alarm_cfg.ticks = 10;
    alarm_cfg.callback = hw_tick;
    alarm_cfg.user_data = &alarm_cfg;
    if (!counter_set_channel_alarm(counter_dev, 0, &alarm_cfg))
        printk("HWCounter: Alarm set\r\n");
    else
        printk("HWCounter: Error setting alarm\r\n");
    if (!counter_set_guard_period(counter_dev, UINT_MAX / 2, COUNTER_GUARD_PERIOD_LATE_TO_SET))
        printk("HWCounter: Guard period set\r\n");
    else
        printk("HWCounter: Error setting guard period\r\n");
    counter_start(counter_dev);
    printk("Starting Tick-Thread\r\n");
    start_tick_thread();
}