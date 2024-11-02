#include <stdio.h>
#include <stdlib.h>

#define TABLE_MOD 19

unsigned short map[] = {
    [16] = 1,  // Mon
    [5] = 2,   // Tue
    [9] = 3,   // Wed
    [15] = 4,  // Thu
    [0] = 5,   // Fri
    [14] = 6,  // Sat
    [2] = 7    // Sun
};

int main() {
  char buf[3];
  fgets(buf, 3, stdin);

  printf("%hu\n", map[(size_t)((buf[0] * buf[1]) % TABLE_MOD)]);

  return 0;
}
