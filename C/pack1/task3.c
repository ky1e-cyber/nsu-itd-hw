#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

static inline int16_t get_int16() {
  int16_t x;
  scanf("%hi", &x);
  return x;
}

static inline uint16_t get_uint16() {
  uint16_t x;
  scanf("%hu", &x);
  return x;
}

static inline int8_t *get_array(size_t n, int8_t *buf) {
  for (size_t i = 0; i < n; i++) {
    buf[i] = (int8_t)get_int16();
  }

  return buf;
}

int main() {
  uint8_t n = (uint8_t)get_uint16();
  int8_t arr[n]; // VLA goes brrrrrr

  (void)get_array(n, arr);

  int32_t sum = 0;

  for (size_t i = 0; i < n; i++) {
    if (abs(arr[i]) % 2 == 0) {
      sum += arr[i];
    }
  }

  printf("%d\n", sum);

  return 0;
}
