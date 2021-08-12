#include <ssm-platform.h>

unsigned long debug_count = 0;

void ssm_throw(int reason, const char *file, int line, const char *func) {
  /** Translate error codes into parseable trace events. */
  switch (reason) {
  case SSM_EXHAUSTED_ACT_QUEUE:
    SSM_DEBUG_TRACE("ExhaustedActQueue");
    exit(0);
  case SSM_EXHAUSTED_EVENT_QUEUE:
    SSM_DEBUG_TRACE("ExhaustedEventQueue");
    exit(0);
  case SSM_EXHAUSTED_MEMORY:
    SSM_DEBUG_TRACE("ExhaustedMemory");
    exit(0);
  case SSM_EXHAUSTED_PRIORITY:
    SSM_DEBUG_TRACE("ExhaustedPriority");
    exit(0);
  case SSM_INVALID_TIME:
    SSM_DEBUG_TRACE("CrashInvalidTime");
    exit(0);
  case SSM_EXHAUSTED_MICROTICK:
    SSM_DEBUG_TRACE("ExhaustedMicrotick");
    exit(0);
  case SSM_ARITHMETIC_ERROR:
    SSM_DEBUG_TRACE("CrashArithmeticError");
    exit(0);
  default:
    SSM_DEBUG_TRACE("CrashUnforeseen \"Unknown error code: %d\"", reason);
    exit(reason);
  }
}
