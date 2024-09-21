#include <inttypes.h>
#include <stdio.h>

static inline uint16_t ceil_div(uint16_t x, uint16_t y) {
  return (x / y) + (x % y == 0 ? 0 : 1);
}

int main() {
  uint16_t _n;
  scanf("%hu", &_n);
  uint16_t _m, _p, _k, _l;
  scanf("%hu %hu %hu %hu", &_m, &_p, &_k, &_l);

  // literal SSA lmao

  const uint16_t new_num = _n;

  const uint16_t old_num = _m;
  const uint16_t old_entrance = _p;
  const uint16_t old_level = _k;
  const uint16_t total_levels = _l;

  const uint16_t old_abs_level = total_levels * (old_entrance - 1) + old_level;
  const uint16_t flats_on_level = ceil_div(old_num, old_abs_level);

  const uint16_t new_abs_level = ceil_div(new_num, flats_on_level);
  const uint16_t new_enterance = ceil_div(new_abs_level, total_levels);
  const uint16_t new_level =
      (new_abs_level - (new_enterance - 1) * total_levels);

  printf("%hu %hu\n", new_enterance, new_level);

  return 0;
}
