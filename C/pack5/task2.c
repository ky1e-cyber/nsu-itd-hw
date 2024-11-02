#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define null_check(p)                                  \
  {                                                    \
    if (p == NULL) {                                   \
      fprintf(stderr, "Failed to allocate memory.\n"); \
      exit(2);                                         \
    }                                                  \
  }

#define STR_SIZE 8

typedef struct {
  char* flat_array;
  size_t _capacity;
  size_t len;
} strarr_t;

void init_strarr(strarr_t* p) {
  char* arr = (char*)malloc(sizeof(char) * STR_SIZE);
  null_check(arr);
  arr[0] = '\0';
  p->_capacity = 1;
  p->len = 0;
  p->flat_array = arr;
}

void append_strarr(strarr_t* p, char* elem) {
  if (p->len >= p->_capacity) {
    size_t new_cap = p->_capacity * 2;
    assert(new_cap > p->_capacity);
    p->flat_array =
        (char*)realloc(p->flat_array, new_cap * sizeof(char) * STR_SIZE);
    null_check(p->flat_array);
    p->_capacity = new_cap;
  }
  strcpy(p->flat_array + p->len * STR_SIZE, elem);
  p->len++;
}

char* get_strarr(strarr_t* p, size_t ind) {
  return p->flat_array + ind * STR_SIZE;
}

typedef struct {
  enum { NONE = 0, SOME } flag;
  strarr_t some;
} strarr_option_t;

#define ARRS_SIZE (1000000 + 1)
strarr_option_t arrs[ARRS_SIZE] = {0};

void print_strarr(strarr_t* s, size_t ind) {
  for (size_t i = 0; i < s->len; i++) {
    printf("%lu %s\n", ind, get_strarr(s, i));
  }
}

int main() {
  size_t n;
  scanf("%lu", &n);

  for (size_t i = 0; i < n; i++) {
    size_t ind;
    char buf[STR_SIZE];
    scanf("%lu %s", &ind, buf);

    if (arrs[ind].flag == NONE) {
      arrs[ind] = (strarr_option_t){.flag = SOME};
      init_strarr(&(arrs[ind].some));
    }

    append_strarr(&(arrs[ind].some), buf);
  }

  for (size_t i = 0; i < ARRS_SIZE; i++) {
    if (arrs[i].flag == SOME) {
      print_strarr(&(arrs[i].some), i);
    }
  }

  return 0;
}
