#include <stdio.h>
#include <stdlib.h>

#define HIST_SIZE 127

unsigned int hist[HIST_SIZE] = {0};

int main() {
  const size_t buf_size = 1000000;
  char* const text_buf = (char*)malloc(buf_size * sizeof(char));

  while (fgets(text_buf, buf_size, stdin) != NULL) {
    for (char* s = text_buf; *s != '\0'; s++) {
      hist[(size_t)*s]++;
    }
  }

  for (size_t i = 32; i < HIST_SIZE; i++) {
    if (hist[i]) {
      putc((char)i, stdout);
      putc(' ', stdout);
      for (unsigned int j = 0; j < hist[i]; j++) {
        putc('#', stdout);
      }
      putc('\n', stdout);
    }
  }

  return 0;
}
