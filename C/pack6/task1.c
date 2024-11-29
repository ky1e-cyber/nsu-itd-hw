#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  float val;
  int next_ind;
} arraylist_node_t;

typedef struct {
  arraylist_node_t* arr;
  int head_ind;
} arraylist_t;

void arraylist_print(arraylist_t arrlist) {
  int head = arrlist.head_ind;

  while (head != -1) {
    printf("%0.3lf\n", arrlist.arr[head].val);
    head = arrlist.arr[head].next_ind;
  }

  puts("");
}

int main() {
  size_t n;
  int head_ind;
  scanf("%lu %d", &n, &head_ind);

  arraylist_node_t* arr =
      (arraylist_node_t*)malloc(sizeof(arraylist_node_t) * n);

  for (size_t i = 0; i < n; i++) {
    float val;
    int nxt;
    scanf("%f %d", &val, &nxt);
    arr[i] = (arraylist_node_t){.val = val, .next_ind = nxt};
  }

  arraylist_t arrlist = {.arr = arr, .head_ind = head_ind};

  arraylist_print(arrlist);

  return 0;
}
