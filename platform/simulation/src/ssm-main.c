/** A minimal driver main loop definition. */
#include <ssm-platform.h>

int main(void) {
  ssm_activate_program();

  for (;;) {
    ssm_tick();
    if (ssm_next_event_time() == SSM_NEVER)
      break;
  }
}
