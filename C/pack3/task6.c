#include <stdio.h>
#include <stdlib.h>

static inline size_t read_num(char* istr, int* out) {
  if (!(*istr >= '0' && *istr <= '9'))
    return 0;

  *out = 0;
  size_t i = 0;
  for (; istr[i] >= '0' && istr[i] <= '9'; i++) {
    *out *= 10;
    *out += istr[i] - '0';
  }

  return i;
}

int read_time(char* istr, int* ohours, int* ominutes, int* oseconds) {
  *ohours = 0;

  int h = 0;
  size_t read1 = read_num(istr, &h);

  if (read1 == 0 || h > 23)
    return 0;
  if (istr[read1] != ':')
    return 0;

  *ohours = h;

  int m = 0;
  size_t read2 = read_num(istr + read1 + 1, &m);

  if (read2 == 0 || m > 59)
    return 0;

  *ohours = h;
  if (ominutes != NULL)
    *ominutes = m;

  if (istr[read1 + read2 + 1] == ':') {
    int s = 0;
    size_t read3 = read_num(istr + read1 + read2 + 2, &s);

    if (read3 == 0 || s > 59)
      return 0;

    if (oseconds != NULL)
      *oseconds = s;
  }

  return 1;
}

int main() {
  char time[8 + 1];
  fgets(time, 9, stdin);

  int hrs = 0;
  int mnts = 0;
  int seconds = 0;

  int res1 = read_time(time, &hrs, &mnts, &seconds);
  printf("%d %d %d %d\n", res1, res1 ? hrs : -1, res1 ? mnts : -1,
         res1 ? seconds : -1);

  int res2 = read_time(time, &hrs, &mnts, NULL);
  printf("%d %d %d\n", res2, res2 ? hrs : -1, res2 ? mnts : -1);

  int res3 = read_time(time, &hrs, NULL, NULL);
  printf("%d %d", res3, res3 ? hrs : -1);

  return 0;
}
