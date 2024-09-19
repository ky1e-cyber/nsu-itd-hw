#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>

static const char *YES = "YES";
static const char *NO = "NO";

uint16_t get_uint16() {
  uint16_t x;
  scanf("%hu", &x);
  return x;
}

bool is_prime(uint16_t x) {
  if (x == 1) {
    return false;
  } else if (x <= 3) {
    return true;
  }

  for (size_t i = 2; i * i <= x; i++) {
    if (x % i == 0)
      return false;
  }

  return true;
}

int main() {
  const uint16_t n = get_uint16();
  puts(is_prime(n) ? YES : NO);
  return 0;
}
