#include <stdio.h>

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

static size_t mystrlen(char* str) {
  size_t i = 0;
  for (; str[i] != '\0'; i++)
    ;
  return i;
}

char* concat(char* pref, char* suff) {
  size_t ind = mystrlen(pref);

  while (*suff != '\0') {
    pref[ind++] = *suff;
    suff++;
  }

  pref[ind] = '\0';
  return pref + ind;
}

char result[10000 * 100 + 1] = {'\0'};

int main() {
  size_t n;
  scanf("%lu", &n);
  (void)fgetc(stdin);

  size_t len = read_string(result);
  char* cword_ptr = result;

  for (size_t i = 0; i < n - 1; i++) {
    char str[101];
    size_t _len = read_string(str);
    (void)concat(cword_ptr, str);

    cword_ptr = cword_ptr + len;
    len = _len;
  }

  printf("%s\n", result);

  return 0;
}
