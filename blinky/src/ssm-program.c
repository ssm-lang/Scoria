

#include <stdbool.h>
#include <stdio.h>

#include <peng.h>
#include <peng-led.h>
#include <peng-button.h>
#include <peng-program.h>

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
    
/*    switch(act->pc) {
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
    }*/

    switch(act->pc) {
        case 0:
            // while
            L0:
            if (!(true)) goto L1;
            sensitize((sv_t *)button0, &act->trig1);
            sensitize((sv_t *)button1, &act->trig2);
            sensitize((sv_t *)button2, &act->trig3);
            sensitize((sv_t *)button3, &act->trig4);
            act->pc = 1;
            return;
        case 1:
            desensitize(&act->trig1);
            desensitize(&act->trig2);
            desensitize(&act->trig3);
            desensitize(&act->trig4);
            if(event_on((sv_t*)button0)) {
              assign_led(&led0, act->priority, led0.value ? false : true);
            }
            if(event_on((sv_t*)button1)) {
              assign_led(&led1, act->priority, led1.value ? false : true);
            }
            if(event_on((sv_t*)button2)) {
              assign_led(&led2, act->priority, led2.value ? false : true);
            }
            if(event_on((sv_t*)button3)) {
              assign_led(&led3, act->priority, led3.value ? false : true);
            }
            goto L0;
            L1:
        case 3:
        leave((act_t *) act, sizeof(act_flipled_t));
    }
}