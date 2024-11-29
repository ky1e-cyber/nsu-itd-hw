#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define STRVAL_SIZE 7

typedef struct {
  char val[STRVAL_SIZE];
  int next_ind;
} arraylist_node_t;

typedef struct {
  arraylist_node_t* arr;
  size_t arr_len;
  size_t arr_size;
  int head_ind;
} arraylist_t;

arraylist_node_t* arraylist_get_node(arraylist_t* list, int ind) {
  int arr_ind = list->head_ind;

  for (int cnt = 0; cnt < ind; cnt++) {
    int nxt = list->arr[arr_ind].next_ind;
    if (nxt == -1) {
      return NULL;
    }
    arr_ind = nxt;
  }

  return list->arr + arr_ind;
}

// Insert after ind
arraylist_node_t* arraylist_insert(arraylist_t* list, char val[STRVAL_SIZE], int ind) {
  
  arraylist_node_t* node = arraylist_get_node(list, ind);
  if (node == NULL) {
    return NULL;
  }

  
}

int main() {
  return 0;
}
