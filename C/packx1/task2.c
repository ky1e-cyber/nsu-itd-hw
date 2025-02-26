#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// Теоретический минимум и алгоритмы цифровой подписи, 2010, с. 56-57.
static inline uint64_t pow_modulo(uint64_t a, uint64_t p, uint64_t m) {
  if (p == 0)
    return 1;

  uint64_t s = 1;
  uint64_t v = p;
  uint64_t c = a;

  while (v > 0) {
    if (v % 2 != 0) {
      s = (s * c) % m;
      v = (v - 1) / 2;
    } else {
      v = v / 2;
    }

    c = (c * c) % m;
  }

  return s;
}

int main() {
  uint32_t t, m;
  scanf("%u %u", &t, &m);
  // __asume(m >= 2);
  for (uint32_t i = 0; i < t; i++) {
    uint32_t a;
    scanf("%u", &a);
    // __asume(a < m);
    long long int b = (long long int)pow_modulo(a, m - 2, m);
    printf("%lli\n", (((a * b) - 1) % m) == 0 ? b : -1);
  }

  return 0;
}
