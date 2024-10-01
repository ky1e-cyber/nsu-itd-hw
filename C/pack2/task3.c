#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

uint32_t hist[10001];

int main() {
  size_t n;
  scanf("%lu", &n);

  uint16_t* arr = (uint16_t*)malloc(sizeof(uint16_t) * n);

  for (size_t i = 0; i < n; i++)
    scanf("%hu", arr + i);

  for (size_t i = 0; i < n; i++)
    hist[arr[i]]++;

  for (uint16_t i = 1; i < 10001; i++) {
    if (hist[i])
      printf("%hu: %u\n", i, hist[i]);
  }

  return 0;
}
