/** A minimal driver main loop definition. */
#include <ssm-platform.h>

int main(void) {
  ssm_activate(
      ssm_entry_point(&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH));

  for (;;) {
    ssm_tick();
    if (ssm_next_event_time() == SSM_NEVER)
      break;
  }
}
