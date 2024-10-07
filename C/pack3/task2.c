#include <stdio.h>

void reverse(char* start, size_t len) {
  if (len == 0)
    return;

  char* end = start + (len - 1);

  while (start <= end) {
    char tmp = *start;
    *start = *end;
    *end = tmp;
    start++;
    end--;
  }
}

char str[101];

static inline size_t read_string(char* str) {
  char c = fgetc(stdin);
  size_t count = 0;

  while (c != '\n') {
    *str = c;
    str++;
    count++;
    c = fgetc(stdin);
  }

  *str = '\0';
  return count;
}

int main() {
  unsigned short n;
  scanf("%hu", &n);
  (void)fgetc(stdin);

  for (unsigned short i = 0; i < n; i++) {
    size_t len = read_string(str);
    reverse(str, len);
    printf("%s\n", str);
  }

  return 0;
}
