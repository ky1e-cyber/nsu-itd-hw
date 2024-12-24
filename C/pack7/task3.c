#include <stdio.h>
#include <stdlib.h>

void str_to_bytes(char* str, unsigned short* dest, size_t size) {
  if (size == 0)
    return;

  unsigned short num = str[0] - '0';

  for (size_t i = 1; i < size; i++) {
    if (i % 8 == 0) {
      dest[(i / 8) - 1] = num;
      num = 0;
    }

    unsigned short digit = str[i] - '0';
    num += digit << (i % 8);
  }

  dest[(size - 1) / 8] = num;
}

int main() {
  size_t n;
  scanf("%lu", &n);
  fgetc(stdin);

  char* str = (char*)malloc(sizeof(char) * n);

  char c = fgetc(stdin);

  for (size_t i = 0; c == '0' || c == '1'; c = fgetc(stdin)) {
    str[i++] = c;
  }

  size_t bytes_size = (n / 8 + (n % 8 == 0 ? 0 : 1));
  unsigned short* bytees =
      (unsigned short*)malloc(bytes_size * sizeof(unsigned short));

  str_to_bytes(str, bytees, n);

  for (size_t i = 0; i < bytes_size; i++)
    printf("%hu ", bytees[i]);

  puts("");

  return 0;
}
