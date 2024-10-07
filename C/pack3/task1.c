#include <stdio.h>

int main() {
  unsigned short n;
  scanf("%hu", &n);

  for (unsigned short i = 0; i < n; i++) {
    unsigned short h, m, s;
    scanf("%hu %hu %hu", &h, &m, &s);
    printf("%02hu:%02hu:%02hu\n", h, m, s);

  }

  return 0;
}
