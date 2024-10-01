#include <stdio.h>
#include <stdlib.h>

static char buf[10000 + 1] = {'\0'};

static inline void read_input(char* buf) {
  char c = getc(stdin);

  for (size_t i = 0; c != '\n'; i++) {
    buf[i] = c;
    c = getc(stdin);
  }
}

unsigned int count_tail(char* buf, unsigned int count) {
  while (*buf == '.') {
    buf++;
  }

  if (*buf == '\0')
    return count;

  while (*buf != '.' && *buf != '\0') {
    buf++;
  }

  return count_tail(buf, count + 1);
}

static inline unsigned int count(char* buf) {
  return count_tail(buf, 0);
}

int main() {
  read_input(buf);
  printf("%u\n", count(buf));
  return 0;
}
