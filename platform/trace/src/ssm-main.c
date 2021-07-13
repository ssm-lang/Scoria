#include <ssm-platform.h>

extern uint16_t event_queue_len;

int main(void) {
  ssm_activate(
      ssm_entry_point(&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH));

  for (;;) {
    ssm_tick();
    if (ssm_next_event_time() == SSM_NEVER)
      break;
    SSM_DEBUG_TRACE("now %ld\n", ssm_next_event_time());
    SSM_DEBUG_TRACE("eventqueuesize %d\n", event_queue_len);
  }
}
