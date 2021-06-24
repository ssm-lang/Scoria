#ifndef _PENG_PLATFORM_H
#define _PENG_PLATFORM_H

#include <stdio.h>
#include <stdlib.h>

#define NANOSECOND_TICKS(x) (x)
#define MICROSECOND_TICKS(x) ((x)*1000L)
#define MILLISECOND_TICKS(x) ((x)*1000000L)
#define SECOND_TICKS(x) ((x)*1000000000L)
#define MINUTE_TICKS(x) ((x)*60000000000L)
#define HOUR_TICKS(x) ((x)*3600000000000L)

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
#define AT __FILE__ ":" TOSTRING(__LINE__)

#define DEBUG_LIMIT 10000

extern unsigned long debug_count;

#define DEBUG_PRINT(...)                                                       \
  {                                                                            \
    if (debug_count >= DEBUG_LIMIT) {                                          \
      exit(1);                                                                 \
    }                                                                          \
    debug_count++;                                                             \
    printf(__VA_ARGS__);                                                       \
  }

#define DEBUG_ASSERT(assertion, ...)                                           \
  do {                                                                         \
    if (!(assertion)) {                                                        \
      printf(__VA_ARGS__);                                                     \
      exit(1);                                                                 \
    }                                                                          \
  } while (0)

#endif
