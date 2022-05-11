#include <ssm.h>
#include <ssm-internal.h>
#include <ssm-test-platform.h>
ssm_value_t glob0;
ssm_value_t glob1;
ssm_value_t glob2;
ssm_value_t glob3;
ssm_value_t glob4;
typedef struct {
            ssm_act_t act;
        } act_fun0_t;
ssm_act_t *ssm_enter_fun0(struct ssm_act *parent, ssm_priority_t priority, ssm_depth_t depth);
void step_fun0(struct ssm_act *act);
ssm_act_t *ssm_enter_fun0(struct ssm_act *parent, ssm_priority_t priority, ssm_depth_t depth)
{
    act_fun0_t *cont = container_of(ssm_enter(sizeof(*cont), step_fun0, parent, priority, depth), act_fun0_t, act);
    
    return &cont->act;
}
void step_fun0(struct ssm_act *act)
{
    act_fun0_t *cont = container_of(act, act_fun0_t, act);
    
    SSM_DEBUG_TRACE("ActStepBegin \"fun0\"\n");
    SSM_DEBUG_MICROTICK();
    switch (act->pc) {
        
      case 0:
        ;
        //  dropping references before terminating;
        ssm_leave(&cont->act, sizeof(act_fun0_t));
    }
}
void ssm_program_init(void)
{
    glob0 = ssm_new_sv(ssm_marshal(0));
    glob1 = ssm_new_sv(ssm_marshal(0));
    glob2 = ssm_new_sv(ssm_marshal(0));
    glob3 = ssm_new_sv(ssm_marshal(0));
    glob4 = ssm_new_sv(ssm_marshal(0));
    ssm_dup(glob0);
    ssm_dup(glob1);
    ssm_dup(glob2);
    ssm_dup(glob3);
    ssm_dup(glob4);
    ssm_activate(ssm_enter_fun0(&ssm_top_parent, SSM_ROOT_PRIORITY + 0 * (1 << 0), SSM_ROOT_DEPTH - 0));
}
void ssm_program_exit(void)
{
    ssm_drop(glob0);
    ssm_drop(glob1);
    ssm_drop(glob2);
    ssm_drop(glob3);
    ssm_drop(glob4);
}
