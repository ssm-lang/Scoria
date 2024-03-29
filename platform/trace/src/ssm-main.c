#include <ssm-platform.h>

extern uint16_t event_queue_len;

int main(void) {
  ssm_program_initialize();

  for (;;) {
    ssm_tick();
    if (ssm_next_event_time() == SSM_NEVER)
      break;
    SSM_DEBUG_TRACE("DriverEventQueueStatus %u %lu", event_queue_len, ssm_next_event_time());
  }
  SSM_DEBUG_TRACE("TerminatedOk");
}
