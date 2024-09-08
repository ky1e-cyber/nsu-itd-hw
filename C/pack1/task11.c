#include <inttypes.h>
#include <stdio.h>

int main() {
  FILE* inp = fopen("input.txt", "r");

  uint32_t n;
  fscanf(inp, "%lu", (unsigned long*)&n);

  uint32_t max_side = 2;

  uint64_t sum = (max_side % 2) == 0 ? ((max_side + 1) / 2) * max_side
                                     : (max_side / 2) * (n + 1);

  return 0;
}
