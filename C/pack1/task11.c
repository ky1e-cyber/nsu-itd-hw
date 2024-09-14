#include <inttypes.h>
#include <stdio.h>
#include <math.h>

// using inline to eliminate unnecessary
// function call

static inline uint64_t count_cubes(uint32_t n) {
  uint32_t cbr_n = (uint32_t)cbrtf(n);

  uint64_t count = 0;
  for (uint32_t i = 1; i <= cbr_n ; i++) {
    for (uint32_t j = i; j * j <= n / i; j++) {
      uint32_t c = (n / ((uint64_t)i * j));
      count += c >= j ? c - j + 1 : 0;
    }
  }

  return count;
}

int main() {
  uint32_t n;
  scanf("%u", &n);
  printf("%llu", count_cubes(n));
  return 0;
}
