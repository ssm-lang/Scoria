#include <peng-led.h>
#include <peng-button.h>
#include "peng-platform.h"
#include "peng.h"
#include "formatters.h"
#include <stdio.h>
#include <stdint.h>
sv_led_t led0;
sv_button_t *buttons[1];
sv_button_t button0;
extern peng_time_t now;
peng_time_t limit = ULONG_MAX;
static int _add(int a, int b)
{
    return a + b;
}
typedef struct {
            stepf_t *step;
            struct act *caller;
            uint16_t pc;
            uint16_t children;
            priority_t priority;
            depth_t depth;
            bool scheduled;
            sv_uint64_t time;
            sv_int32_t v0;
            trigger_t trig1;
        } act_delayprocess_t;
act_delayprocess_t *enter_delayprocess(act_t *caller, priority_t priority, depth_t depth, uint64 time);
void step_delayprocess(act_t *gen_act);
typedef struct {
            stepf_t *step;
            struct act *caller;
            uint16_t pc;
            uint16_t children;
            priority_t priority;
            depth_t depth;
            bool scheduled;
            trigger_t trig1;
        } act_testprogram2_t;
act_testprogram2_t *enter_testprogram2(act_t *caller, priority_t priority, depth_t depth);
void step_testprogram2(act_t *gen_act);
typedef struct {
            stepf_t *step;
            struct act *caller;
            uint16_t pc;
            uint16_t children;
            priority_t priority;
            depth_t depth;
            bool scheduled;
            sv_led_t *r;
            sv_led_t v0;
        } act_toggleprocess_t;
act_toggleprocess_t *enter_toggleprocess(act_t *caller, priority_t priority, depth_t depth, sv_led_t *r);
void step_toggleprocess(act_t *gen_act);
act_delayprocess_t *enter_delayprocess(act_t *caller, priority_t priority, depth_t depth, uint64 time)
{
    act_t *gen_act = enter(sizeof(act_delayprocess_t), step_delayprocess, caller, priority, depth);
    act_delayprocess_t *act = (act_delayprocess_t *) gen_act;
    
    initialize_uint64(&act->time);
    act->time.value = time;
    initialize_int32(&act->v0);
    act->trig1.act = gen_act;
    return act;
}
void step_delayprocess(act_t *gen_act)
{
    act_delayprocess_t *act = (act_delayprocess_t *) gen_act;
    
    /* FIXME: remove cast */
    switch (gen_act->pc) {
        
      case 0:
        ;
        assign_int32(&act->v0, gen_act->priority, 0);
        later_int32(&act->v0, now + act->time.value, 1);
        sensitize((sv_t *) &act->v0, &act->trig1);
        gen_act->pc = 1;
        return;
        
      case 1:
        ;
        desensitize(&act->trig1);
        
      case 2:
        ;
        dequeue_event((sv_t *) &act->v0);
        leave(gen_act, sizeof(act_delayprocess_t));
        return;
    }
    printf("Error: Unreachable\n");
    assert(0);
}
act_testprogram2_t *enter_testprogram2(act_t *caller, priority_t priority, depth_t depth)
{
    act_t *gen_act = enter(sizeof(act_testprogram2_t), step_testprogram2, caller, priority, depth);
    act_testprogram2_t *act = (act_testprogram2_t *) gen_act;
    
    act->trig1.act = gen_act;
    return act;
}
void step_testprogram2(act_t *gen_act)
{
    act_testprogram2_t *act = (act_testprogram2_t *) gen_act;
    
    /* FIXME: remove cast */
    switch (gen_act->pc) {
        
      case 0:
        ;
        sensitize((sv_t *) &button0, &act->trig1);
        gen_act->pc = 1;
        return;
        
      case 1:
        ;
        desensitize(&act->trig1);
        sensitize((sv_t *) &button0, &act->trig1);
        gen_act->pc = 2;
        return;
        
      case 2:
        ;
        desensitize(&act->trig1);
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 5 * (uint64) 16000000));
        gen_act->pc = 3;
        return;
        
      case 3:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 4;
        return;
        
      case 4:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 5;
        return;
        
      case 5:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 6;
        return;
        
      case 6:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 7;
        return;
        
      case 7:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 8;
        return;
        
      case 8:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 9;
        return;
        
      case 9:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 10;
        return;
        
      case 10:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 11;
        return;
        
      case 11:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 12;
        return;
        
      case 12:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 13;
        return;
        
      case 13:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 14;
        return;
        
      case 14:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 15;
        return;
        
      case 15:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 16;
        return;
        
      case 16:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 17;
        return;
        
      case 17:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 18;
        return;
        
      case 18:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 19;
        return;
        
      case 19:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 20;
        return;
        
      case 20:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 21;
        return;
        
      case 21:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 22;
        return;
        
      case 22:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 23;
        return;
        
      case 23:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 24;
        return;
        
      case 24:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 25;
        return;
        
      case 25:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 26;
        return;
        
      case 26:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 27;
        return;
        
      case 27:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 28;
        return;
        
      case 28:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 29;
        return;
        
      case 29:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 30;
        return;
        
      case 30:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 31;
        return;
        
      case 31:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 32;
        return;
        
      case 32:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 33;
        return;
        
      case 33:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 34;
        return;
        
      case 34:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 35;
        return;
        
      case 35:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 36;
        return;
        
      case 36:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 37;
        return;
        
      case 37:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 38;
        return;
        
      case 38:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 39;
        return;
        
      case 39:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 40;
        return;
        
      case 40:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 41;
        return;
        
      case 41:
        ;
        DEBUG_PRINT("fork toggleprocess\n");
        fork_routine((act_t *) enter_toggleprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                   &led0));
        gen_act->pc = 42;
        return;
        
      case 42:
        ;
        DEBUG_PRINT("fork delayprocess\n");
        fork_routine((act_t *) enter_delayprocess(gen_act, act->priority + 0 * (1 << act->depth - 0), act->depth - 0,
                                                  (uint64) 4000000));
        gen_act->pc = 43;
        return;
        
      case 43:
        ;
        
      case 44:
        ;
        leave(gen_act, sizeof(act_testprogram2_t));
        return;
    }
    printf("Error: Unreachable\n");
    assert(0);
}
act_toggleprocess_t *enter_toggleprocess(act_t *caller, priority_t priority, depth_t depth, sv_led_t *r)
{
    act_t *gen_act = enter(sizeof(act_toggleprocess_t), step_toggleprocess, caller, priority, depth);
    act_toggleprocess_t *act = (act_toggleprocess_t *) gen_act;
    
    act->r = r;
    initialize_led(&act->v0);
    return act;
}
void step_toggleprocess(act_t *gen_act)
{
    act_toggleprocess_t *act = (act_toggleprocess_t *) gen_act;
    
    /* FIXME: remove cast */
    switch (gen_act->pc) {
        
      case 0:
        ;
        assign_led(&act->v0, gen_act->priority, act->r->value);
        if (act->v0.value == true) {
            assign_led(act->r, gen_act->priority, false);
        } else {
            assign_led(act->r, gen_act->priority, true);
        }
        
      case 1:
        ;
        dequeue_event((sv_t *) &act->v0);
        leave(gen_act, sizeof(act_toggleprocess_t));
        return;
    }
    printf("Error: Unreachable\n");
    assert(0);
}
void initializeIO()
{
    initialize_ledIO(&led0, 0);
    led0.value = false;
    initialize_buttonIO(&button0, 0, 0);
    button0.value = false;
    buttons[0] = &button0;
}
extern act_t top;
void initializeCont()
{
    fork_routine((act_t *) enter_testprogram2((act_t *) &top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT));
}
extern void get_hw_now(uint64_t *time);
void handle_driver_msg(ll_driver_msg_t *msg)
{
    if (msg->msg_type == 0 && msg->driver_id == -1)
        printk("woke up, now it's time to call tick\n");
    if (msg->msg_type == 1 && (false || msg->driver_id == 0)) {
        uint64_t hw_now;
        
        get_hw_now(&hw_now);
        
        sv_button_t *button = buttons[msg->driver_id];
        
        later_button(button, hw_now, msg->data);
    }
}
