#include <inttypes.h>
#include <stdio.h>

static inline void read_seq(int8_t* buf, uint16_t size) {
  for (uint16_t i = 0; i < size; i++) {
    int16_t x;
    scanf("%hi", &x);
    buf[i] = (int8_t)x;
  }
}

int main() {
  uint16_t _n;
  scanf("%hu", &_n);
  const uint16_t n = _n;

  int8_t arr[n];
  read_seq(arr, n);

  uint16_t neg_count = 0;
  uint16_t pos_count = 0;
  uint16_t zero_count = 0;

  for (uint16_t i = 0; i < n; i++) {
    if (arr[i] > 0) {
      pos_count++;
    } else if (arr[i] < 0) {
      neg_count++;
    } else {
      zero_count++;
    }
  }

  double neg = (double)neg_count / n;
  double zero = (double)zero_count / n;
  double pos = (double)pos_count / n;
  printf("%0.5lf %0.5lf %0.5lf", neg, zero, pos);

  return 0;
}
