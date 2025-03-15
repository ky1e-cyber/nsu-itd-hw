#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static inline int64_t min(int64_t x, int64_t y) {
  return x < y ? x : y;
}

static int64_t modulo(int64_t a, uint64_t b) {
  int64_t c = a % b;
  return c >= 0 ? c : c + b;
}

static inline int64_t ext_gcd(int64_t a, int64_t b) {
  int64_t x = 1, y = 0, x1 = 0, y1 = 1, a1 = a, b1 = b;

  while (b1 != 0) {
    int64_t q = a1 / b1;

    int64_t x_tmp = x1;
    x1 = x - q * x1;
    x = x_tmp;

    int64_t y_tmp = y1;
    y1 = y - q * y1;
    y = y_tmp;

    int64_t b_tmp = b1;
    b1 = a1 - q * b1;
    a1 = b_tmp;
  }

  return x;
}

static int64_t inv_gcd(uint64_t a, uint64_t m) {
  return ext_gcd((int64_t)a, (int64_t)m);
}

static uint64_t mprod(int64_t a, int64_t b, uint64_t m) {
  int64_t t = ((long double)a * b) / m;
  int64_t res = ((int64_t)(a * b - t * m)) % (int64_t)m;

  return res < 0 ? (res + m) : res;
}

int64_t cmt(const uint64_t* const modulos,
            const uint64_t* const reminders,
            size_t size) {
  uint64_t p = 1;
  for (size_t i = 0; i < size; i++)
    p *= modulos[i];

  int64_t x = 0;
  for (size_t i = 0; i < size; i++) {
    uint64_t d = p / modulos[i];
    int64_t inv_d = inv_gcd(d % modulos[i], modulos[i]);
    x += d * (mprod(inv_d, reminders[i], modulos[i]));
  }

  return min(modulo(x, p), x);
}

int main() {
  size_t k;
  scanf("%lu", &k);

  uint64_t* ms = (uint64_t*)malloc(sizeof(uint64_t) * k);
  uint64_t* rs = (uint64_t*)malloc(sizeof(uint64_t) * k);

  for (size_t i = 0; i < k; i++)
    scanf("%llu", ms + i);
  for (size_t i = 0; i < k; i++)
    scanf("%llu", rs + i);

  printf("%lli\n", (long long)cmt(ms, rs, k));

  return 0;
}
