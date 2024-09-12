#include <inttypes.h>
#include <stdio.h>

static inline uint64_t count_cubes2(uint32_t n) {
  uint64_t count = 0;

  for (uint32_t i = 1; i <= n; i++) {
    for (uint32_t k = i; k <= (n / ((uint64_t)i * i)); k++) {
      count += (n / ((uint64_t)i * k)) - i; // REDO
    }
  }

  return count;
}

// uint64_t count_cubes(uint32_t n) {
//   uint64_t count = 0;
//   for (uint32_t i = 1; i <= n; i++) {
//     uint32_t j = i;
//     while ((i * j) <= n) {
//       uint64_t c = (n / ((uint64_t)j * i));
//       if (c >= (j - 1)) {
//         count += c - (j - 1);
//       } else {
//         break;
//       }
//       j++;
//     }
//   }

//   return count;
// }

int main() {
  FILE* inp = fopen("input.txt", "r");
  FILE* out = fopen("output.txt", "w");

  uint32_t n;
  fscanf(inp, "%u", &n);
  fprintf(out, "%llu", count_cubes2(n));

  fclose(inp);
  fclose(out);

  return 0;
}
