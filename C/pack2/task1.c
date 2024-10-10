#include <stdio.h>
#include <stdlib.h>

static inline unsigned short count_smaller(unsigned short* ys,
                                           size_t size,
                                           unsigned short x) {
  unsigned short cnt = 0;
  for (size_t i = 0; i < size; i++) {
    if (ys[i] < x)
      cnt++;
  }

  return cnt;
}

// size == |xs| == |counter_buff|
unsigned short count_inverses(unsigned short* xs,
                              size_t size,
                              unsigned short* counter_buff) {
  if (size <= 1)
    return 0;

  size_t mid = size / 2;

  unsigned short invs = count_inverses(xs, mid, counter_buff) +
                        count_inverses(xs + mid, size - mid, counter_buff + mid);

  for (size_t i = 0; i < mid; i++) {
    unsigned short c = count_smaller(xs + mid, size - mid, xs[i]);
    invs += c;
    counter_buff[i] += c;
  }

  return invs;
}

int main() {
  size_t n;
  scanf("%lu", &n);

  unsigned short* arr = (unsigned short*)malloc(sizeof(unsigned short) * n);
  unsigned short* cnt_buff = (unsigned short*)calloc(n, sizeof(unsigned short));

  for (size_t i = 0; i < n; i++)
    scanf("%hu", arr + i);

  (void)count_inverses(arr, n, cnt_buff);

  for (size_t i = 0; i < n - 1; i++)
    printf("%hu ", cnt_buff[i]);

  printf("%hu\n", cnt_buff[n - 1]);

  return 0;
}
