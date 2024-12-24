#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef double f64_t;

static inline f64_t xored(uint16_t p, uint16_t q, uint64_t num) {
  f64_t div = ((f64_t)p) / q;
  uint64_t* casted = (uint64_t*)&div;
  uint64_t xored = (*casted) ^ num;
  f64_t* casted_back = (f64_t*)&xored;
  return *casted_back;
}

int main() {
  size_t n;
  scanf("%lu", &n);

  for (size_t i = 0; i < n; i++) {
    uint16_t p, q;
    scanf("%hu", &p);
    fgetc(stdin);
    scanf("%hu", &q);
    for (size_t j = 0; j < 5; j++)
      fgetc(stdin);

    uint64_t num;
    scanf("%llx", &num);

    printf("%0.15g\n", xored(p, q, num));
  }

  return 0;
}
