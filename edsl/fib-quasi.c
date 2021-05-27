#include "peng-platform.h"
#include "peng.h"
#include <stdio.h>
#include <stdint.h>
extern void dequeue_event(sv_t *var);
extern peng_time_t now;
extern int can_schedule(sv_t *var);
peng_time_t limit = ULONG_MAX;
static int __add(int a, int b)
{
    return a + b;
}
#define ADD(a, b) __add(a, b)
typedef struct {
            stepf_t *step;
            struct act *caller;
            uint16_t pc;
            uint16_t children;
            priority_t priority;
            depth_t depth;
            bool scheduled;
            int32 n;
            int32 *r;
            sv_int32_t r2;
            sv_int32_t r1;
        } act_myfib_t;
act_myfib_t *enter_myfib(act_myfib_t *caller, priority_t priority, depth_t depth, int32 n, int32 *r);
void step_myfib(act_t *gen_act);
typedef struct {
            stepf_t *step;
            struct act *caller;
            uint16_t pc;
            uint16_t children;
            priority_t priority;
            depth_t depth;
            bool scheduled;
            int32 *r1;
            int32 *r2;
            int32 *r;
        } act_mysum_t;
act_mysum_t *enter_mysum(act_mysum_t *caller, priority_t priority, depth_t depth, int32 *r1, int32 *r2, int32 *r);
void step_mysum(act_t *gen_act);
typedef struct {
            stepf_t *step;
            struct act *caller;
            uint16_t pc;
            uint16_t children;
            priority_t priority;
            depth_t depth;
            bool scheduled;
            int32 *r;
            trigger_t trig1;
        } act_waitSingle_t;
act_waitSingle_t *enter_waitSingle(act_waitSingle_t *caller, priority_t priority, depth_t depth, int32 *r);
void step_waitSingle(act_t *gen_act);
act_myfib_t *enter_myfib(act_myfib_t *caller, priority_t priority, depth_t depth, int32 n, int32 *r)
{
    act_t gen_act = act_enter(sizeof(act_myfib_t), step_myfib, caller, priority, depth);
    act_myfib_t act = (act_myfib_t *) gen_act;
    
    act->n = n;
    act->r = r;
    initialize_int32(&act->r2);
    initialize_int32(&act->r1);
    return act;
}
void step_myfib(act_t *gen_act)
{
    act_myfib_t *act = (act_myfib_t *) gen_act;
    act_myfib_t *act = container_of(gen_act, act_myfib_t, act);
    
    /* FIXME: remove cast */
    switch (gen_act->pc) {
        
      case 0:
        ;
        assign_int32(&act->r1, gen_act->priority, 0);
        assign_int32(&act->r2, gen_act->priority, 0);
        if (act->n.value < 2) {
            later_int32(act->r, 1, 1);
        } else {
            fork_routine((act_t *) enter_myfib(act, act->priority + 4, act->depth - 2, act->n.value - 1, &act->r1));
            fork_routine((act_t *) enter_myfib(act, act->priority + 8, act->depth - 2, act->n.value - 2, &act->r2));
            fork_routine((act_t *) enter_mysum(act, act->priority + 12, act->depth - 2, &act->r1, &act->r2, act->r));
            gen_act->pc = 1;
            return;
            
          case 1:
            ;
        }
        
      case 2:
        ;
        dequeue_event((sv_t *) &act->r2);
        dequeue_event((sv_t *) &act->r1);
        leave((act_t *) act, sizeof(act_myfib_t));
        /* FIXME: remove cast */
        return;
    }
    printf("Error: Unreachable\n");
    assert(0);
}
act_mysum_t *enter_mysum(act_mysum_t *caller, priority_t priority, depth_t depth, int32 *r1, int32 *r2, int32 *r)
{
    act_t gen_act = act_enter(sizeof(act_mysum_t), step_mysum, caller, priority, depth);
    act_mysum_t act = (act_mysum_t *) gen_act;
    
    act->r1 = r1;
    act->r2 = r2;
    act->r = r;
    return act;
}
void step_mysum(act_t *gen_act)
{
    act_mysum_t *act = (act_mysum_t *) gen_act;
    
    /* FIXME: remove cast */
    switch (gen_act->pc) {
        
      case 0:
        ;
        fork_routine((act_t *) enter_waitSingle(act, act->priority + 2, act->depth - 1, act->r1));
        fork_routine((act_t *) enter_waitSingle(act, act->priority + 4, act->depth - 1, act->r2));
        gen_act->pc = 1;
        return;
        
      case 1:
        ;
        assign_int32(&act->v1, gen_act->priority, act->r1->value);
        assign_int32(&act->v2, gen_act->priority, act->r2->value);
        later_int32(act->r, 1, add(act->v1.value, act->v2.value));
        
      case 2:
        ;
        leave((act_t *) act, sizeof(act_mysum_t));
        /* FIXME: remove cast */
        return;
    }
    printf("Error: Unreachable\n");
    assert(0);
}
act_waitSingle_t *enter_waitSingle(act_waitSingle_t *caller, priority_t priority, depth_t depth, int32 *r)
{
    act_t gen_act = act_enter(sizeof(act_waitSingle_t), step_waitSingle, caller, priority, depth);
    act_waitSingle_t act = (act_waitSingle_t *) gen_act;
    
    act->r = r;
    act->trig1 = gen_act;
    return act;
}
void step_waitSingle(act_t *gen_act)
{
    act_waitSingle_t *act = (act_waitSingle_t *) gen_act;
    
    /* FIXME: remove cast */
    switch (gen_act->pc) {
        
      case 0:
        ;
        sensitize((sv_t *) act->r, &act->trig1);
        gen_act->pc = 1;
        return;
        
      case 1:
        ;
        desensitize(&act->trig1);
        
      case 2:
        ;
        leave((act_t *) act, sizeof(act_waitSingle_t));
        /* FIXME: remove cast */
        return;
    }
    printf("Error: Unreachable\n");
    assert(0);
}
void main(void)
{
    act_t top = {.step =top_return};
    sv_int32_t r;
    
    initialize_int32(&r);
    r.value = 0;
    fork_routine((act_t *) enter_myfib(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT, 13, &r));
    tick();
    DEBUG_PRINT("now %lu eventqueuesize %d\n", now, event_queue_len);
    for (; ;) {
        now = next_event_time();
        if (now == NO_EVENT_SCHEDULED)
            break;
        tick();
        DEBUG_PRINT("now %lu eventqueuesize %d\n", now, event_queue_len);
    }
    printf("result r %ld\n", (long) r.value);
}
