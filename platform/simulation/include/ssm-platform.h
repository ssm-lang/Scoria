#ifndef _PENG_PLATFORM_H
#define _PENG_PLATFORM_H

#include "ssm.h"

#include <stdio.h>
#include <stdlib.h>

extern struct ssm_act *(*ssm_entry_point)(struct ssm_act *, ssm_priority_t,
                                          ssm_depth_t);

#define SSM_DEBUG_ASSERT(assertion, ...)                                       \
    do if (!(assertion)){                                                     \
      printf(__VA_ARGS__);                                                     \
      exit(1);                                                                 \
  } while (0)

#define SSM_DEBUG_TRACE(...) do; while(0)
#define SSM_DEBUG_MICROTICK() do; while(0)

#endif
