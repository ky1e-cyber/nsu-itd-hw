#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

static inline bool is_lower(char c) {
  return c >= 'a' && c <= 'z';
}

static inline bool is_upper(char c) {
  return c >= 'A' && c <= 'Z';
}

static inline bool is_digit(char c) {
  return c >= '0' && c <= '9';
}

int calc_letters(char* istr, int* olowercnt, int* ouppercnt, int* odigitcnt) {
  int lowercnt = 0;
  int uppercnt = 0;
  int digitcnt = 0;
  int len = 0;

  for (; *istr != '\0'; istr++) {
    len++;
    if (is_lower(*istr)) {
      lowercnt++;
    } else if (is_upper(*istr)) {
      uppercnt++;
    } else if (is_digit(*istr)) {
      digitcnt++;
    }
  }

  *olowercnt = lowercnt;
  *ouppercnt = uppercnt;
  *odigitcnt = digitcnt;

  return len;
}

int main() {
  char str[102];


  int line = 1;

  char* p = fgets(str, 101, stdin);
  while (p != NULL) {
    int lowercnt = 0;
    int uppercnt = 0;
    int digitcnt = 0;
    int len = calc_letters(str, &lowercnt, &uppercnt, &digitcnt);

    printf(
        "Line %i has %i chars: %i are letters (%i lower, %i upper), %i are "
        "digits.\n",
        line, len - 1, lowercnt + uppercnt, lowercnt, uppercnt, digitcnt);

    line++;
    p = fgets(str, 101, stdin);
  }

  return 0;
}
