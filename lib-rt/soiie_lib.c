#include "soiie_lib.h"

long int parse_or_die(const char* str);

// we want this to be inlined so the array gets promoted to registers when the loop is unrolled
void __attribute__((always_inline)) get_params(int argc, char* argv[], long int params[]) {
  if (argc != PARAM_COUNT + 1) {
    printf("Error: expected %d arguments, got %d\n", PARAM_COUNT, argc - 1);
    exit(EXIT_FAILURE);
  }

  for (int i = 0; i < PARAM_COUNT; i++) {
    params[i] = parse_or_die(argv[i+1]);
  }
}

void /* __attribute__((noinline)) */ print(long int x) {
  printf("%ld\n", x);
}

// noinline so the loop in get_params gets unrolled
long int __attribute__((noinline)) parse_or_die(const char* str) {
  char* end;
  errno = 0;
  long int val = strtol(str, &end, 0);
  if ((errno == ERANGE && (val == LONG_MAX || val == LONG_MIN))
      || (errno != 0 && val == 0)
      || *end != '\0') {
    printf("Error: invalid integer: %s\n", str);
    exit(EXIT_FAILURE);
  }
  return val;
}
