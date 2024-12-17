#include <assert.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define STRVAL_SIZE 8

typedef struct {
  uint32_t key;
  char value[STRVAL_SIZE];
} item_t;

static inline long long radix_sort_get_key(long long key, unsigned int radix) {
  return (key / radix) % 10;
}

// radix : [1, 10, 100 ...]
void radix_sort_counting(item_t* arr,
                         item_t* dest,
                         long long size,
                         unsigned int digit) {
  unsigned int radix = pow(10, digit - 1);

  unsigned int cnt[11] = {0};

  for (long long i = 0; i < size; i++)
    cnt[radix_sort_get_key(arr[i].key, radix)]++;

  for (int i = 1; i < 11; i++)
    cnt[i] += cnt[i - 1];

  for (long long i = size - 1; i >= 0; i--) {
    long long key = radix_sort_get_key(arr[i].key, radix);
    dest[cnt[key] - 1] = arr[i];
    cnt[key]--;
  }
}

static inline void buf_swap(item_t** buf1, item_t** buf2) {
  item_t* tmp = *buf1;
  *buf1 = *buf2;
  *buf2 = tmp;
}

static inline void buf_cpy(item_t* dst, item_t* src, size_t size) {
  // pls do memcpy
  for (size_t i = 0; i < size; i++)
    dst[i] = src[i];
}

item_t* radix_sort(item_t* arr, size_t size) {
  item_t* buf1 = arr;
  item_t* buf2 = (item_t*)malloc(size * sizeof(item_t));
  if (buf2 == NULL)
    return NULL;

  unsigned int max_width = 1;
  for (size_t i = 0; i < size; i++) {
    unsigned int width = log10f(arr[i].key) + 1;
    if (width > max_width)
      max_width = width;
  }

  for (unsigned int digit = 1; digit <= max_width; digit++) {
    radix_sort_counting(buf1, buf2, size, digit);
    buf_swap(&buf1, &buf2);
  }

  if (max_width % 2 != 0) {
    buf_cpy(buf2, buf1, size);
    buf_swap(&buf1, &buf2);
  }

  free(buf2);

  return arr;
}

static inline void read_item(item_t* it) {
  scanf("%u", &it->key);
  fgetc(stdin);  // eat space
  scanf("%s", it->value);
}

int main() {
  size_t n;
  scanf("%lu", &n);

  item_t* arr = (item_t*)malloc(n * sizeof(item_t));

  for (size_t i = 0; i < n; i++)
    read_item(arr + i);

  radix_sort(arr, n);

  for (size_t i = 0; i < n; i++)
    printf("%u %s\n", arr[i].key, arr[i].value);

  free(arr);

  return 0;
}
