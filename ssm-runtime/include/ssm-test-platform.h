#ifndef SSM_TEST_PLATFORM
#define SSM_TEST_PLATFORM

#include <stdio.h>
#include <ssm-internal.h>
#include <ssm.h>
#include <string.h>

void ssm_program_init(void);
void ssm_program_exit(void);

#define SSM_DEBUG_TRACE(...) printf(__VA_ARGS__)

#define SSM_DEBUG_MICROTICK() do {} while(0)

void ssm_throw(enum ssm_error reason, const char *file, int line,
               const char *func) {
  printf("SSM error at %s:%s:%d: reason: %d\n", file, func, line, reason);
  exit(1);
}

#define MAX_PAGES 2048
static void *pages[MAX_PAGES];
static size_t allocated_pages = 0;

static void *alloc_page(void) {
  if (allocated_pages >= MAX_PAGES) {
    SSM_THROW(SSM_EXHAUSTED_MEMORY);
    exit(3);
  }
  void *m = pages[allocated_pages++] = malloc(SSM_MEM_PAGE_SIZE);
  memset(m, 0, SSM_MEM_PAGE_SIZE);
  return m;
}

static void *alloc_mem(size_t size) { return malloc(size); }

static void free_mem(void *mem, size_t size) { free(mem); }

static void print_help(char *prog) {
  printf("Usage: %s [OPTION]... [--] [ARG]...\n", prog);
  printf("\n");
  printf("This help menu describes usage for arguments common across all "
         "examples.\n");
  printf("Unrecognized arguments or arguments past `--', whichever appears "
         "sooner,\n");
  printf("are passed to the specific example.\n");
  printf("\n");
  printf("Options:\n");
  printf("  -l, --limit <time>  limit the simluation to <time>, in seconds.\n");
  printf("  -h, --help          show this help menu.\n");
  printf("\n");
}

char **ssm_init_args;

int main(int argc, char *argv[]) {
  ssm_mem_init(alloc_page, alloc_mem, free_mem);

  size_t stop_at_s = 20;
  char *prog = *argv;

  for (argv++; *argv; argv++) {
    if (strcmp(*argv, "-h") == 0 || strcmp(*argv, "--help") == 0) {
      print_help(prog);
      return 0;
    } else if (strcmp(*argv, "-l") == 0 || strcmp(*argv, "--limit") == 0) {
      if (*++argv == NULL) {
        printf("Error: insufficient arguments following `%s'.\n", *--argv);
        print_help(prog);
        return 2;
      }
      if (strcmp(*argv, "0") == 0) {
        stop_at_s = 0;
      } else {
        stop_at_s = atoi(*argv);
        if (stop_at_s == 0) {
          printf("Error: could not parse time limit: `%s'.\n", *argv);
          print_help(prog);
          return 2;
        }
      }
    } else if (strcmp(*argv, "--") == 0) {
      argv++;
      break;
    } else {
      break;
    }
  }

  ssm_init_args = argv;

  ssm_time_t stop_at = stop_at_s == 0 ? SSM_NEVER : stop_at_s * SSM_SECOND;

  ssm_program_init();

  ssm_tick();

  while (ssm_next_event_time() != SSM_NEVER && ssm_now() < stop_at)
    ssm_tick();

  printf("%s: simulated %lu seconds\n", prog, ssm_now() / SSM_SECOND);

  ssm_program_exit();

  for (size_t p = 0; p < allocated_pages; p++)
    free(pages[p]);

  return 0;
}

#endif // SSM_TEST_PLATFORM
