#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  int16_t _a;
  uint16_t _b;
  scanf("%hi %hu", &_a, &_b);
  const int16_t a = _a;
  const uint16_t b = _b;

  const float div = (float)a / b;

  const int16_t div_floor = (int16_t)floor(div);
  const int16_t div_ceil = (int16_t)ceilf(div);
  const int16_t div_round = (int16_t)round(div);
  const uint16_t remainder = a > 0 ? abs(a) % b : b - (abs(a) % b);

  printf("%hi %hi %hi %hu\n", div_floor, div_ceil, div_round, remainder);

  return 0;
}
