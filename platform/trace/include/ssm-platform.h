#ifndef _SSM_PLATFORM_H
#define _SSM_PLATFORM_H

#include "ssm.h"

#include <stdio.h>
#include <stdlib.h>

extern struct ssm_act *(*ssm_entry_point)(struct ssm_act *, ssm_priority_t,
                                          ssm_depth_t);

extern void ssm_throw(int reason, const char *file, int line, const char *func);

enum { SSM_EXHAUSTED_MICROTICK = SSM_PLATFORM_ERROR, SSM_ARITHMETIC_ERROR };

#define SSM_DEBUG_LIMIT 10000
extern unsigned long debug_count;

#define SSM_DEBUG_ASSERT(assertion, ...)                                       \
  do                                                                           \
    if (!(assertion)) {                                                        \
      printf(__VA_ARGS__);                                                     \
      exit(1);                                                                 \
    }                                                                          \
  while (0)

#define SSM_DEBUG_TRACE(...) printf(__VA_ARGS__), printf("\n")

#define SSM_DEBUG_MICROTICK()                                                  \
  if (debug_count++ >= SSM_DEBUG_LIMIT)                                        \
  SSM_THROW(SSM_EXHAUSTED_MICROTICK)

#endif
