#include <inttypes.h>
#include <stdio.h>

static inline uint8_t get_int8() {
  int16_t x;
  scanf("%hi", &x); // omg hiii :3
  return (int8_t)x;
}

/**
 * validate_input(int8_t, int8_t, int8_t) -
 *     Validates input according to the rules.
 * @arg1: x axis size.
 * @arg2: y axis size.
 * @arg3: z axis size.
 * Return:
 *     -1 if @arg1 is invalid
 *     -2 if @arg2 is invalid
 *     -3 if @arg3 is invalid
 *      0 else
 */
static inline int8_t validate_input(int8_t x, int8_t y, int8_t z) {
  return x <= 0 ? -1 : (y <= 0 ? -2 : (z <= 0 ? -3 : 0)); // kys
}

static inline int32_t surface_area(int8_t x, int8_t y, int8_t z) {
  return (2 * (int32_t)x * y) + (2 * (int32_t)x * z) + (2 * (int32_t)z * y);
}

int main() {
  const int8_t x = get_int8();
  const int8_t y = get_int8();
  const int8_t z = get_int8();
  const int8_t val = validate_input(x, y, z);

  printf("%d\n", !val ? surface_area(x, y, z) : (int32_t)val);

  return 0;
}
