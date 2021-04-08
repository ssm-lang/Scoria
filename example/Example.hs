{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Example where

import BinderAnn.Monadic
import Core
import Frontend

example :: Ref Int -> SSM ()
example = box "example" ["r"] $ \r -> do
    v0 <- var true'
    v1 <- changed v0
    after (int 13) v0 v1

{-
#include "peng-platform.h"
#include "peng.h"
#include <stdio.h>

typedef struct {
    /* Generic procedure fields */
    ACTIVATION_RECORD_FIELDS;
    /* Procedure specific fields */
    sv_int_t v0; // generated name
    sv_bool_t v1; // generated name
    sv_bool_t v2; // generated name
    sv_bool_t v3; // generated name
    sv_int_t v4; // generated name
    sv_int_t v5; // generated name
    sv_bool_t v6; // generated name
    trigger_t trig1;
    trigger_t trig2;
} act_main_t;

act_main_t *enter_main(act_t *caller, uint32_t priority, uint8_t depth);
void step_main(act_t *gen_act);

act_main_t *enter_main(act_t *caller, uint32_t priority, uint8_t depth) {
    act_main_t *act = (act_main_t *) enter(sizeof(act_main_t), step_main, caller, priority, depth);
    act->trig1.act = (act_t *) act;
    act->trig2.act = (act_t *) act;
    return act;
};

void step_main(act_t *gen_act) {
    act_main_t *act = (act_main_t *) gen_act;
    switch (act->pc) {
        case 0:
            initialize_int(&act->v0);
            assign_int(&act->v0, act->priority, (((-3)) + ((-4))) + ((-(-2))));
            initialize_bool(&act->v1);
            assign_bool(&act->v1, act->priority, (((-2)) - ((-1))) < (((-3)) * (1)));
            assign_bool(&act->v2, act->priority, event_on((sv_t *)&act->v0));
            later_bool(&act->v1, now + (((-5)) * ((-5))) + (1), ((0) < ((-4))) == ((act->v2.value) == (false)));
            assign_bool(&act->v3, act->priority, act->v1.value);
            if (!((((-5)) + ((-6))) < ((6) * ((-5))))) goto L0;
            if (!(act->v2.value)) goto L1;
            if (!(act->v3.value)) goto L3;
            assign_int(&act->v0, act->priority, (((-6)) + (0)) - (((-6)) * ((-7))));
            goto L4;

        L3:
            sensitize((sv_t *) &act->v1, &act->trig1);
            sensitize((sv_t *) &act->v0, &act->trig2);
            act->pc = 1;
            return;
        case 1:
            desensitize(&act->trig1);
            desensitize(&act->trig2);

        L4:
            goto L2;

        L1:
            if (!((((-7)) * ((-5))) < ((5) + (6)))) goto L5;
            assign_bool(&act->v1, act->priority, act->v2.value);
            goto L6;

        L5:
            initialize_int(&act->v4);
            assign_int(&act->v4, act->priority, (3) - (7));

        L6:

        L2:
            assign_int(&act->v5, act->priority, act->v0.value);

        L0:
            assign_bool(&act->v6, act->priority, act->v1.value);
            assign_int(&act->v0, act->priority, ((-4)) * ((-6)));

        leave((act_t *) act, sizeof(act_main_t));
    }
}

void top_return(act_t *act) {
    return;
}

void main(void) {
    act_t top = { .step = top_return };
    // set up input references
    fork_routine((act_t *) enter_main(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT));
    tick();
    printf("now %lu eventqueuesize %d\n", now, event_queue_len);
    while(1) {
        now = next_event_time();
        if(now == NO_EVENT_SCHEDULED) {
            break;
        }
        tick();
        printf("now %lu eventqueuesize %d\n", now, event_queue_len);
    }
}


code generator  interpreter     
--------------  -----------     
Instant 0 1     Instant 0 1     
Event 26 True   Event 26 False   <----- different


Procedure main
v0 = NewRef ((-3 + -4) + (- -2))
v1 = NewRef ((-2 - -1) < (-3 * 1))
v2 = @v0
After ((-5 * -5) + 1) v1 = ((0 < -4) == (v2 == False))
v3 = GetRef v1
If ((-5 + -6) < (6 * -5))
Then
  If v2
  Then
    If v3
    Then
      SetRef v0 = ((-6 + 0) - (-6 * -7))
    Else
      Wait [v1, v0]
  Else
    If ((-7 * -5) < (5 + 6))
    Then
      SetRef v1 = v2
    Else
      v4 = NewRef (3 - 7)
  v5 = GetRef v0
v6 = GetRef v1
SetRef v0 = (-4 * -6)
Result

Procedure main
v0 = NewRef -5
v1 = NewRef False
v2 = @v0
After (26) v1 = False == (v2 == False))
v3 = GetRef v1
If False
Then
  If v2
  Then
    If v3
    Then
      SetRef v0 = 36
    Else
      Wait [v1, v0]
  Else
    If False
    Then
      SetRef v1 = v2
    Else
      v4 = NewRef -4
  v5 = GetRef v0
v6 = GetRef v1
SetRef v0 = 24
Result

-}