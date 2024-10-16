#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 1001

unsigned int duration_table[TABLE_SIZE] = {0};
unsigned int fcount_table[TABLE_SIZE] = {0};

static inline void print_tb_column_line(size_t width) {
  for (size_t i = 0; i < width + 2; i++) {
    putc('-', stdout);
  }
}

static inline void print_tb_line(size_t id_width,
                                 size_t fcount_width,
                                 size_t duratuin_width) {
  putc('+', stdout);
  print_tb_column_line(id_width);
  putc('+', stdout);
  print_tb_column_line(fcount_width);
  putc('+', stdout);
  print_tb_column_line(duratuin_width);
  putc('+', stdout);
  putc('\n', stdout);
}

static inline void print_tb_single_value(char* val,
                                         size_t val_size,
                                         size_t max_width) {
  const size_t sp = max_width - val_size + 1;

  for (size_t i = 0; i < sp; i++)
    putc(' ', stdout);

  printf("%s", val);
  putc(' ', stdout);
}

static inline void print_tb_values(unsigned int id,
                                   size_t max_id_width,
                                   unsigned int fcount,
                                   size_t max_fcount_width,
                                   unsigned int duration,
                                   size_t max_duration_width) {
  char id_buf[5];
  char fcount_buf[6];
  char duration_buf[11];

  (void)sprintf(id_buf, "%u", id);
  (void)sprintf(fcount_buf, "%u", fcount);
  (void)sprintf(duration_buf, "%u", duration);

  const size_t id_width = strlen(id_buf);
  const size_t fcount_width = strlen(fcount_buf);
  const size_t duration_width = strlen(duration_buf);

  putc('|', stdout);
  print_tb_single_value(id_buf, id_width, max_id_width);
  putc('|', stdout);
  print_tb_single_value(fcount_buf, fcount_width, max_fcount_width);
  putc('|', stdout);
  print_tb_single_value(duration_buf, duration_width, max_duration_width);
  puts("|");
}

// better than log10 i swear
size_t uint_width(unsigned int x) {
  if (x < 10) {
    return 1;
  } else if (x < 100) {
    return 2;
  } else if (x < 1000) {
    return 3;
  } else if (x < 10000) {
    return 4;
  } else if (x < 100000) {
    return 5;
  } else if (x < 1000000) {
    return 6;
  } else if (x < 10000000) {
    return 7;
  } else if (x < 100000000) {
    return 8;
  } else if (x < 1000000000) {
    return 9;
  } else if (x <= UINT_MAX) {
    return 10;
  }
  return 1;
}

int main() {
  int n;
  scanf("%u", &n);

  size_t max_id_width = 1;
  size_t max_fcount_width = 1;
  size_t max_duration_width = 1;

  for (int i = 0; i < n; i++) {
    unsigned int id, duration;
    scanf("%u %u", &id, &duration);
    fcount_table[id] += 1;
    duration_table[id] += duration;

    size_t id_width = uint_width(id);
    if (id_width > max_id_width)
      max_id_width = id_width;
  }

  for (size_t i = 0; i < TABLE_SIZE; i++) {
    if (fcount_table[i]) {
      size_t fcount_width = uint_width(fcount_table[i]);
      size_t duration_width = uint_width(duration_table[i]);

      max_fcount_width =
          fcount_width > max_fcount_width ? fcount_width : max_fcount_width;

      max_duration_width = duration_width > max_duration_width
                               ? duration_width
                               : max_duration_width;
    }
  }

  for (size_t i = 0; i < TABLE_SIZE; i++) {
    if (fcount_table[i]) {
      print_tb_line(max_id_width, max_fcount_width, max_duration_width);
      print_tb_values(i, max_id_width, fcount_table[i], max_fcount_width,
                      duration_table[i], max_duration_width);
    }
  }

  print_tb_line(max_id_width, max_fcount_width, max_duration_width);

  return 0;
}
