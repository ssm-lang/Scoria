#include "ssm-program.h"
typedef struct {
  struct act act;
  u64_svt *ref2;
  struct trigger trig1;
} act_fun1_t;

struct act *enter_fun1(struct act *caller, priority_t priority, depth_t depth,
                       u64_svt *ref2);
void step_fun1(struct act *actg);

struct act *enter_fun1(struct act *caller, priority_t priority, depth_t depth,
                       u64_svt *ref2) {
  struct act *actg =
      act_enter(sizeof(act_fun1_t), step_fun1, caller, priority, depth);
  act_fun1_t *acts = container_of(actg, act_fun1_t, act);

  acts->ref2 = ref2;
  acts->trig1.act = actg;
  return actg;
}

void step_fun1(struct act *actg) {
  act_fun1_t *acts = container_of(actg, act_fun1_t, act);

  switch (actg->pc) {

  case 0:;
    while (true) {
      later_u64(acts->ref2, get_now() + (u64)2, (u64)1);
      sensitize(&acts->ref2->sv, &acts->trig1);
      actg->pc = 1;
      return;

    case 1:;
      desensitize(&acts->trig1);
      later_u64(acts->ref2, get_now() + (u64)2, (u64)0);
      sensitize(&acts->ref2->sv, &acts->trig1);
      actg->pc = 2;
      return;

    case 2:;
      desensitize(&acts->trig1);
    }

  case 3:;
    act_leave(actg, sizeof(act_fun1_t));
    return;
  }
  assert(0);
}

void top_return(struct act *act) { return; }
