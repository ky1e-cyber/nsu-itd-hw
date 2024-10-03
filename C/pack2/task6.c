#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

uint32_t set[3126] = {0};  // (10**5) / 32 == 3125
                            // +1 for zero

uint32_t get_shift(uint32_t x) {
  return x % 32;
}

size_t get_ind(uint32_t x) {
  return x / 32;
}

uint32_t lookup(uint32_t x) {
  size_t int_ind = get_ind(x);
  uint32_t shift = get_shift(x);
  return set[int_ind] & ((uint32_t)1 << shift);
}

void insert(uint32_t x) {
  size_t int_ind = get_ind(x);
  uint32_t shift = get_shift(x);
  set[int_ind] |= (uint32_t)1 << shift;
}

int main() {
  size_t a_size;
  scanf("%lu", &a_size);

  uint32_t* a = (uint32_t*)malloc(sizeof(uint32_t) * a_size);
  for (size_t i = 0; i < a_size; i++) {
    scanf("%ui", a + i);
  }

  size_t b_size;
  scanf("%lu", &b_size);

  for (size_t i = 0; i < b_size; i++) {
    uint32_t b_i;
    scanf("%u", &b_i);
    insert(b_i);
  }

  size_t insert_ind = 0;

  for (size_t i = 0; i < a_size; i++) {
    if (!lookup(a[i])) {
      a[insert_ind] = a[i];
      insert_ind++;
      insert(a[i]);
    }
  }

  const size_t new_size = insert_ind;

  printf("%lu\n", new_size);
  if (new_size > 0) {
    for (size_t i = 0; i < new_size - 1; i++) {
      printf("%u ", a[i]);
    }
    printf("%u\n", a[new_size - 1]);
  }

  free(a);
  return 0;
}
