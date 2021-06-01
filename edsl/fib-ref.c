#include "peng-platform.h"
#include "peng.h"
#include <stdio.h>

extern void dequeue_event(sv_t *var);

#ifdef DEBUG
#include <stdint.h>
uint64_t limit = ULONG_MAX;
#endif
#ifdef DEBUG

extern peng_time_t now;
extern int can_schedule(sv_t *var);
#define SCHEDULE(schedule_fun, var, then, value)    \
if(now >= then) {                                   \
  printf("bad after\n");                            \
  exit(1);                                          \
}                                                   \
if(!can_schedule((sv_t *) var)) {                   \
  printf("eventqueue full\n");                      \
  exit(1);                                          \
}                                                   \
(*schedule_fun)(var, then, value);

#else

#define SCHEDULE(schedule_fun, var, then, value) \
(*schedule_fun)(var, then, value);

#endif
#ifdef DEBUG

extern int can_fork();
#define FORK(act, new_depth)     \
if((int8_t) new_depth < 0) {     \
  printf("negative depth\n");    \
  exit(1);                       \
}                                \
if(!can_fork()) {                \
  printf("contqueue full\n");    \
  exit(1);                       \
}                                \
fork_routine(act);

#else

#define FORK(act, new_depth) \
fork_routine(act);

#endif
#ifdef DEBUG

int add(int a, int b) { return a + b; }

#define ADD(a,b) add(a,b)

#else

#define ADD(a,b) (a + b)

#endif
typedef struct {
    ACTIVATION_RECORD_FIELDS;
    sv_int_t n;
    sv_int_t* r;
    sv_int_t r1;
    sv_int_t r2;
} act_myfib_t;

typedef struct {
    ACTIVATION_RECORD_FIELDS;
    sv_int_t* r1;
    sv_int_t* r2;
    sv_int_t* r;
    sv_int_t v1;
    sv_int_t v2;
} act_mysum_t;

typedef struct {
    ACTIVATION_RECORD_FIELDS;
    sv_int_t* r;
    trigger_t trig1;
} act_waitSingle_t;

act_myfib_t* enter_myfib(act_t* caller, uint32_t priority, uint8_t depth, int n, sv_int_t* r);
void step_myfib(act_t* gen_act);

act_mysum_t* enter_mysum(act_t* caller, uint32_t priority, uint8_t depth, sv_int_t* r1, sv_int_t* r2, sv_int_t* r);
void step_mysum(act_t* gen_act);

act_waitSingle_t* enter_waitSingle(act_t* caller, uint32_t priority, uint8_t depth, sv_int_t* r);
void step_waitSingle(act_t* gen_act);


act_myfib_t* enter_myfib(act_t* caller, uint32_t priority, uint8_t depth, int n, sv_int_t* r) {
    act_myfib_t* act = (act_myfib_t*) enter(sizeof(act_myfib_t), step_myfib, caller, priority, depth);
    initialize_int(&act->n);
    assign_int(&act->n, act->priority, n);
    act->r = r;
    initialize_int(&act->r2);
    initialize_int(&act->r1);
    return act;
}

act_mysum_t* enter_mysum(act_t* caller, uint32_t priority, uint8_t depth, sv_int_t* r1, sv_int_t* r2, sv_int_t* r) {
    act_mysum_t* act = (act_mysum_t*) enter(sizeof(act_mysum_t), step_mysum, caller, priority, depth);
    act->r1 = r1;
    act->r2 = r2;
    act->r = r;
    return act;
}

act_waitSingle_t* enter_waitSingle(act_t* caller, uint32_t priority, uint8_t depth, sv_int_t* r) {
    act_waitSingle_t* act = (act_waitSingle_t*) enter(sizeof(act_waitSingle_t), step_waitSingle, caller, priority, depth);
    act->r = r;
    act->trig1.act = (act_t *) act;
    return act;
}

void step_myfib(act_t* gen_act) {
    act_myfib_t* act = (act_myfib_t*) gen_act;
    switch(act->pc) {
        case 0:
            assign_int(&act->r1, act->priority, 0);
            assign_int(&act->r2, act->priority, 0);
            if (!((act->n.value < 2))) goto L0;
            SCHEDULE(&later_int, act->r, now + 1UL, 1);
            goto L1;
            
            L0:
            DEBUG_PRINT("fork myfib myfib mysum\n");
            {
                uint8_t new_depth = act->depth - 2;
                uint32_t pinc = 1 << new_depth;
                uint32_t new_priority = act->priority;
                FORK((act_t *) enter_myfib((act_t *) act, new_priority, new_depth, (act->n.value - 1), &act->r1), new_depth);
                new_priority += pinc;
                FORK((act_t *) enter_myfib((act_t *) act, new_priority, new_depth, (act->n.value - 2), &act->r2), new_depth);
                new_priority += pinc;
                FORK((act_t *) enter_mysum((act_t *) act, new_priority, new_depth, &act->r1, &act->r2, act->r), new_depth);
            }
            act->pc = 1;
            return;
        case 1:
            
            L1:
        case 2:
        dequeue_event((sv_t *)&act->r2);
        dequeue_event((sv_t *)&act->r1);
        leave((act_t *) act, sizeof(act_myfib_t));
    }
}

void step_mysum(act_t* gen_act) {
    act_mysum_t* act = (act_mysum_t*) gen_act;
    switch(act->pc) {
        case 0:
            DEBUG_PRINT("fork waitSingle waitSingle\n");
            {
                uint8_t new_depth = act->depth - 1;
                uint32_t pinc = 1 << new_depth;
                uint32_t new_priority = act->priority;
                FORK((act_t *) enter_waitSingle((act_t *) act, new_priority, new_depth, act->r1), new_depth);
                new_priority += pinc;
                FORK((act_t *) enter_waitSingle((act_t *) act, new_priority, new_depth, act->r2), new_depth);
            }
            act->pc = 1;
            return;
        case 1:
            assign_int(&act->v1, act->priority, act->r1->value);
            assign_int(&act->v2, act->priority, act->r2->value);
            SCHEDULE(&later_int, act->r, now + 1UL, add(act->v1.value, act->v2.value));
        case 2:
        leave((act_t *) act, sizeof(act_mysum_t));
    }
}

void step_waitSingle(act_t* gen_act) {
    act_waitSingle_t* act = (act_waitSingle_t*) gen_act;
    switch(act->pc) {
        case 0:
            sensitize((sv_t *)act->r, &act->trig1);
            act->pc = 1;
            return;
        case 1:
            desensitize(&act->trig1);
        case 2:
        leave((act_t *) act, sizeof(act_waitSingle_t));
    }
}


void top_return(act_t* act) {
    return;
}

void main() {
    
    act_t top = { .step = top_return };
    sv_int_t r;
    initialize_int(&r);
    r.value = 0;
    fork_routine((act_t *) enter_myfib(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT, 13, &r));
    tick();
    DEBUG_PRINT("now %lu eventqueuesize %d\n", now, event_queue_len);
    while(1) {
        now = next_event_time();
        if(now == NO_EVENT_SCHEDULED)
            break;
        tick();
        DEBUG_PRINT("now %lu eventqueuesize %d\n", now, event_queue_len);
    }
    printf("result r int %d\n", r.value);
}

