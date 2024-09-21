#include <inttypes.h>
#include <stdio.h>

static inline void read_seq(int16_t* buf, uint8_t size) {
  for (uint8_t i = 0; i < size; i++) {
    scanf("%hi", buf + i);
  }
}

int main() {
  uint16_t _size;
  scanf("%hu", &_size);
  const uint8_t size = (uint8_t)_size;

  int16_t arr[size];
  read_seq(arr, size);

  int16_t min_el = arr[0];
  uint8_t min_ind = 0;

  int16_t max_el = arr[0];
  uint8_t max_ind = 0;

  for (uint8_t i = 1; i < size; i++) {
    if (arr[i] > max_el) {
      max_el = arr[i];
      max_ind = i;
    } else if (arr[i] < min_el) {
      min_el = arr[i];
      min_ind = i;
    }
  }

  printf("%hi %hu %hi %hu", min_el, (uint16_t)min_ind + 1, max_el,
         (uint16_t)max_ind + 1);

  return 0;
}
