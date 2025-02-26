#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint8_t byte_t;

static inline uint32_t key(uint64_t val) {
  return (uint32_t)(val & (uint64_t)UINT32_MAX);
}

static inline byte_t counting_key(uint32_t key, uint8_t digit) {
  key = key >> (8 * (digit - 1));
  return (byte_t)(key & (0xff));
}

void radix_sort_counting(uint64_t* arr,
                         uint64_t* dest,
                         uint32_t size,
                         uint8_t digit) {
  uint32_t cnt[256] = {0};

  for (uint32_t i = 0; i < size; i++)
    cnt[counting_key(key(arr[i]), digit)]++;

  for (uint16_t i = 1; i < 256; i++)
    cnt[i] += cnt[i - 1];

  for (int64_t i = size - 1; i >= 0; i--) {
    byte_t k = counting_key(key(arr[i]), digit);
    dest[cnt[k] - 1] = arr[i];
    cnt[k]--;
  }
}

static inline void buf_swap(uint64_t** buf1, uint64_t** buf2) {
  uint64_t* t = *buf1;
  *buf1 = *buf2;
  *buf2 = t;
}

uint64_t* radix_sort(uint64_t* arr, uint32_t size) {
  if (size <= 1)
    return arr;
  uint64_t* buf1 = arr;
  uint64_t* buf2 = (uint64_t*)malloc(size * 8);
  if (buf2 == NULL)
    return NULL;

  for (uint8_t digit = 1; digit <= 4; digit++) {
    radix_sort_counting(buf1, buf2, size, digit);
    buf_swap(&buf1, &buf2);
  }

  free(buf2);

  return arr;
}

int main() {
  FILE* in = fopen("input.txt", "rb");
  FILE* out = fopen("output.txt", "wb");

  uint32_t n;
  fread(&n, 4, 1, in);

  uint64_t* xs = (uint64_t*)malloc(n * 8);
  fread(xs, 8, n, in);

  (void)radix_sort(xs, n);

  fwrite(xs, 8, n, out);

  free(xs);
  fclose(in);
  fclose(out);

  return 0;
}

