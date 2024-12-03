#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define print_error(errmsg)                        \
  do {                                             \
    fprintf(stderr, "%c%s%c", '[', __func__, ']'); \
    fprintf(stderr, " Failed: ");                  \
    fputs(errmsg, stderr);                         \
  } while (0);

#define STRVAL_SIZE 8
#define ARRAYLIST_INITCAP 16

typedef enum { Err, Ok } result_t;

typedef struct {
  char val[STRVAL_SIZE];
  int next_ind;
} arraylist_init_pair_t;

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

arraylist_t* arraylist_init(arraylist_t*);

arraylist_t* arraylist_init_sized(arraylist_t* list, size_t size) {
  if (size < ARRAYLIST_INITCAP) {
    return arraylist_init(list);
  }
  arraylist_node_t* mem =
      (arraylist_node_t*)malloc(size * sizeof(arraylist_node_t));

  if (mem == NULL) {
    print_error("unable to allocate initial buffer\n");
    return NULL;
  }

  list->arr = mem;
  list->arr_len = 0;
  list->arr_size = ARRAYLIST_INITCAP;
  list->head_ind = -1;

  return list;
}

arraylist_t* arraylist_init(arraylist_t* list) {
  return arraylist_init_sized(list, ARRAYLIST_INITCAP);
}

arraylist_t* arraylist_from_array(arraylist_t* list,
                                  arraylist_init_pair_t* array,
                                  size_t head_ind,
                                  size_t size) {
  list = arraylist_init_sized(list, size);
  if (list == NULL) {
    return NULL;
  }

  for (size_t i = 0; i < size; i++) {
    list->arr[i] = (arraylist_node_t){.next_ind = array[i].next_ind};
    memcpy(&(list->arr[i].val), array[i].val, strlen(array[i].val));
  }

  list->arr_len = size;
  list->head_ind = head_ind;

  return list;
}

void arraylist_free(arraylist_t* list) {
  free(list->arr);
  free(list);
}

size_t arraylist_get_node_array_ind(arraylist_t* list, arraylist_node_t* node) {
  assert(node >= list->arr && node < (list->arr + list->arr_len));
  return (size_t)(node - list->arr);
}

arraylist_node_t* arraylist_get_node(arraylist_t* list, int ind) {
  if (ind == -1) {
    return list->head_ind == -1 ? NULL : &(list->arr[list->head_ind]);
  }

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

arraylist_t* arraylist_grow(arraylist_t* list) {
  size_t new_size = list->arr_size * 2;
  assert(new_size > list->arr_size);
  arraylist_node_t* new_mem = (arraylist_node_t*)realloc(
      list->arr, new_size * sizeof(arraylist_node_t));
  if (new_mem == NULL)
    return NULL;

  list->arr = new_mem;
  list->arr_size = new_size;

  return list;
}

// Insert after ind
arraylist_node_t* arraylist_insert(arraylist_t* list,
                                   char val[STRVAL_SIZE],
                                   int ind) {
  if (ind == -1 && list->head_ind == -1) {
    list->arr_len = 1;
    list->arr[0] = (arraylist_node_t){.next_ind = -1};
    memcpy(&(list->arr[0].val), val, strlen(val) * sizeof(char));
    list->head_ind = 0;

    return list->arr;
  } else if (arraylist_get_node(list, ind) == NULL) {
    // Checking if node exists
    static const char errfmt[] = "no such node with ind %d\n";
    const size_t buf_size = sizeof(errfmt) + 12 * sizeof(char);
    char buf[buf_size];
    snprintf(buf, buf_size, errfmt, ind);
    print_error(buf);

    return NULL;
  }

  // We aren't inserting in empty list and node existst

  if (list->arr_size <= list->arr_len) {
    list = arraylist_grow(list);
    if (list == NULL) {
      print_error("unable to grow array\n");
      return NULL;
    }
  }

  const size_t new_arr_ind = list->arr_len;
  list->arr_len++;

  list->arr[new_arr_ind] = (arraylist_node_t){.next_ind = -1};
  memcpy(&(list->arr[new_arr_ind].val), val, strlen(val) * sizeof(char));

  if (ind == -1) {
    list->arr[new_arr_ind].next_ind = list->head_ind;
    list->head_ind = new_arr_ind;
  } else {
    arraylist_node_t* node = arraylist_get_node(list, ind);
    int node_nxt = node->next_ind;
    node->next_ind = new_arr_ind;
    list->arr[new_arr_ind].next_ind = node_nxt;
  }

  return list->arr + new_arr_ind;
}

result_t arraylist_remove(arraylist_t* list, int ind) {
  arraylist_node_t* node = NULL;

  if (ind == -1) {
    if (list->head_ind == -1) {
      print_error("trying to delete non existing head\n");
      return Err;
    }

    const int nxt = list->arr[list->head_ind].next_ind;
    list->head_ind = nxt;

    return Ok;
  } else if ((node = arraylist_get_node(list, ind)) == NULL) {
    // Checking if node exists
    static const char errfmt[] = "no such node with ind %d\n";
    const size_t buf_size = sizeof(errfmt) + 12 * sizeof(char);
    char buf[buf_size];
    snprintf(buf, buf_size, errfmt, ind);
    print_error(buf);

    return Err;
  }

  int nxt = node->next_ind;
  if (nxt == -1) {
    print_error("trying to delete non existing node");
    return Err;
  }

  int new_nxt = list->arr[nxt].next_ind;
  node->next_ind = new_nxt;

  return Ok;
}

void read_till_space(char* buf) {
  char c;
  while ((c = fgetc(stdin)) != ' ')
    *(buf++) = c;

  *buf = '\0';
}

int main() {
  int t;
  scanf("%d", &t);

  for (int ts = 0; ts < t; ts++) {
    int n, f, q;
    scanf("%d %d %d", &n, &f, &q);

    arraylist_init_pair_t* init =
        (arraylist_init_pair_t*)malloc(n * sizeof(arraylist_init_pair_t));

    for (int j = 0; j < n; j++) {
      (void)fgetc(stdin);
      char buf[STRVAL_SIZE];
      read_till_space(buf);

      int ind;
      scanf("%d", &ind);

      init[j] = (arraylist_init_pair_t){.next_ind = ind};
      memcpy(&(init[j].val), buf, (strlen(buf) + 1) * sizeof(char));

      
    }
  }

  return 0;
}
