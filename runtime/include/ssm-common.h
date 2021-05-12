#ifndef _SSM_COMMON_H
#define _SSM_COMMON_H
/**
 * This header file is common between the scheduler, scheduled variables, and
 * activation records components. It contains type definitions common among
 * them, but delegates to each component to define the specific parts.
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>

/**
 * A year is 31,536,000 seconds, which fits in 25 bits
 * There are 1,000,000 microseconds in a second, which fits in 20 bits
 * If we count microseconds, we only have 12 bits left in 32 bits,
 * enough for 4096 seconds, about 68 minutes or a little more than an hour.
 * 64 bits of microseconds gives 19 bits of years, over 500ky: plenty.
 *
 * Thus, we may assume timestamps never overflow.
 */
typedef uint64_t peng_time_t;

extern peng_time_t now;

typedef uint32_t priority_t;  /* 32-bit thread priority */
typedef uint8_t  depth_t;     /* Index of least significant priority bit */

typedef struct sv sv_t;
typedef struct trigger trigger_t;

#endif /* _SSM_COMMON_H */
