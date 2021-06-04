#ifndef PENG_PROGRAM_
#define PENG_PROGRAM_

#include <peng.h>

typedef struct {
  stepf_t *step;
  struct act *caller;
  uint16_t pc;
  uint16_t children;
  priority_t priority;
  depth_t depth;
  bool scheduled;
  trigger_t trig1;
  trigger_t trig2;
  trigger_t trig3;
  trigger_t trig4;
} act_flipled_t;

act_flipled_t *enter_flipled(act_t *caller, priority_t priority, depth_t depth);
void step_flipled(act_t *gen_act);

#endif