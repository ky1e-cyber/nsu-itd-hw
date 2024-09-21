#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  int32_t _a;
  uint32_t _b;
  scanf("%i %u", &_a, &_b);
  const int32_t a = _a;
  const uint32_t b = _b;

  const float div = (float)a / b;

  const int32_t div_floor = (int32_t)floorf(div);
  const int32_t div_ceil = (int32_t)ceilf(div);
  const int32_t div_round = (int32_t)truncf(div);

  const int32_t remainder = a % (int32_t)b + (a < 0 ? b : 0);

  printf("%i %i %i %i\n", div_floor, div_ceil, div_round, remainder % b);

  return 0;
}
