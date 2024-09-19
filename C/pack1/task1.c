#include <inttypes.h>
#include <stdio.h>

static inline uint8_t get_uint8() {
  uint16_t x;
  scanf("%hu", &x);
  return (uint8_t)x;
}

int main() {
  const uint8_t n = get_uint8();
  const uint16_t sum = ((1 + n) * n) / 2;
  printf("%u\n", sum);
  return 0;
}
