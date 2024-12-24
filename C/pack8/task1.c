#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define swap_bytes(num)                                 \
  ((((num) >> 24) & 0xff) | (((num) << 8) & 0xff0000) | \
   (((num) >> 8) & 0xff00) | (((num) << 24) & 0xff000000))

static inline uint32_t i32_to_u32(int32_t signd) {
  uint32_t* casted = (uint32_t*)&signd;
  return *casted;
}

static inline int32_t u32_to_i32(uint32_t unsignd) {
  int32_t* casted = (int32_t*)&unsignd;
  return *casted;
}

int main() {
  FILE* in = fopen("input.txt", "rb");
  FILE* out = fopen("output.txt", "wb");

  uint32_t n;
  fread(&n, 4, 1, in);

  bool is_bigendian = false;

  if (n > 10000) {
    n = swap_bytes(n);
    is_bigendian = true;
  }

  uint32_t* uns = (uint32_t*)malloc(4 * n);
  fread(uns, 4, n, in);

  int32_t sum = 0;

  for (size_t i = 0; i < n; i++) {
    uint32_t nxt = uns[i];
    sum += u32_to_i32(is_bigendian ? swap_bytes(nxt) : nxt);
  }

  free(uns);

  uint32_t res = i32_to_u32(sum);
  if (is_bigendian)
    res = swap_bytes(res);

  fwrite(&res, 4, 1, out);

  fclose(out);
  fclose(in);

  return 0;
}
