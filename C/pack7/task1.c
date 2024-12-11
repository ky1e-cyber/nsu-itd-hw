#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STRVAL_SIZE 8
#define TABLE_SIZE 10000 + 1

typedef struct _string_list_node {
  char str[STRVAL_SIZE];
  struct _string_list_node* next;
} string_list_node_t;

typedef struct {
  string_list_node_t* head;
  string_list_node_t* maybe_last;
} string_list_t;

typedef struct {
  uint64_t ind;
  string_list_t list;
} item_t;

void string_list_node_init(string_list_node_t* node, char* src) {
  node->next = NULL;
  size_t src_size = (strlen(src) + 1) * sizeof(char);
  (void)memcpy(node->str, src, src_size);
}

string_list_node_t* string_list_node_from_str(char* src) {
  string_list_node_t* mem =
      (string_list_node_t*)malloc(sizeof(string_list_node_t));
  if (mem == NULL)
    return NULL;

  string_list_node_init(mem, src);

  return mem;
}

void string_list_init(string_list_t* lst) {
  lst->head = NULL;
  lst->maybe_last = NULL;
}

void string_list_free(string_list_t* lst) {
  string_list_node_t* node = lst->head;
  while (node != NULL) {
    string_list_node_t* nxt = node->next;
    free(node);
    node = nxt;
  }
}

static inline string_list_node_t* string_list_last(string_list_t* lst) {
  if (lst->maybe_last != NULL)
    return lst->maybe_last;

  string_list_node_t* node = lst->head;
  if (node == NULL)
    return NULL;

  string_list_node_t* nxt = node->next;
  while (nxt != NULL) {
    node = nxt;
    nxt = nxt->next;
  }

  return node;
}

string_list_node_t* string_list_append(string_list_t* lst, char* str) {
  string_list_node_t* new_node = string_list_node_from_str(str);
  if (new_node == NULL)
    return NULL;

  string_list_node_t* last = string_list_last(lst);
  if (last == NULL) {
    lst->head = new_node;
  } else {
    last->next = new_node;
  }

  return new_node;
}

static inline void string_list_print(string_list_t* lst, uint64_t ind) {
  string_list_node_t* node = lst->head;

  while (node != NULL) {
    printf("%llu ", (unsigned long long)ind);
    puts(node->str);
    node = node->next;
  }
}

int main() {
  string_list_t* table =
      (string_list_t*)calloc(TABLE_SIZE, sizeof(string_list_t));

  int n;
  scanf("%d", &n);

  size_t max_ind = 0;

  for (int i = 0; i < n; i++) {
    uint64_t ind;
    (void)scanf("%llu", (unsigned long long int*)&ind);
    if (max_ind < ind)
      max_ind = ind;

    (void)fgetc(stdin);  // eat space
    char buf[STRVAL_SIZE];
    (void)scanf("%s", buf);
    (void)string_list_append(table + ind, buf);
  }

  for (size_t i = 0; i <= max_ind; i++) {
    string_list_print(table + i, i);
  }

  return 0;
}
