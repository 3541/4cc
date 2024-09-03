#include <inttypes.h>
#include <stdio.h>

int main(void) {
  int64_t x = 23;
  uint32_t y = 4;

  printf("int64: %" PRId64 "\n", x);
  printf("uint32: %" PRIu32 "\n", y);
}
