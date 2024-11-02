#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  char name[16];  // имя автора (заканчивается нулём)
  int age;        // возраст автора (сколько лет)
} label_t;

typedef struct {
  int cnt_total;  // сколько всего подписей
  int cnt_long;   // сколько подписей с именами длиннее 10 букв
} namestats_t;

typedef struct {
  int cnt_total;   // сколько всего подписей
  int cnt_adults;  // сколько подписей взрослых (хотя бы 18 лет)
  int cnt_kids;    // сколько подписей детей (меньше 14 лет)
} agestats_t;

void namestats_init(namestats_t* p) {
  p->cnt_long = 0;
  p->cnt_total = 0;
}

void agestats_init(agestats_t* p) {
  p->cnt_total = 0;
  p->cnt_adults = 0;
  p->cnt_kids = 0;
}

void calc_stats(const label_t* arr,
                int cnt,
                namestats_t* onames,
                agestats_t* oages) {
  onames->cnt_total = cnt;
  oages->cnt_total = cnt;

  for (int i = 0; i < cnt; i++) {
    if (strlen(arr[i].name) > 10)
      onames->cnt_long++;
    if (arr[i].age >= 18) {
      oages->cnt_adults++;
    } else if (arr[i].age < 14) {
      oages->cnt_kids++;
    }
  }
}

static inline size_t fgets_untill_space(FILE* stream, char* buf) {
  char c;
  size_t i = 0;
  while ((c = fgetc(stream)) != ' ') {
    buf[i++] = c;
  }
  buf[i] = '\0';
  return i;
}

static inline void fskip(FILE* stream, char skip_till) {
  char c;
  while ((c = fgetc(stream)) != skip_till)
    ;
}

int main() {
  size_t n;
  scanf("%lu", &n);
  label_t* labels = (label_t*)malloc(sizeof(label_t) * n);

  for (size_t i = 0; i < n; i++) {
    (void)fgets_untill_space(stdin, labels[i].name);
    (void)fscanf(stdin, "%d", &labels[i].age);
    fskip(stdin, '\n');
  }

  namestats_t namestats;
  namestats_init(&namestats);
  agestats_t agestat;
  agestats_init(&agestat);

  calc_stats(labels, n, &namestats, &agestat);

  printf(
      "names: total = %d\n"
      "names: long = %d\n"
      "ages: total = %d\n"
      "ages: adult = %d\n"
      "ages: kid = %d\n",
      namestats.cnt_total, namestats.cnt_long, agestat.cnt_total,
      agestat.cnt_adults, agestat.cnt_kids);

  return 0;
}
