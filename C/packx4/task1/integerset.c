#include <assert.h>
#include "base.h"
#include "integerset.h"

static void error_arena() {
  error("Arena alloc error\n");
}

static int IsInChain(ChainedMapNode* node, int value) {
  assert(node->tag == EMPTY || node->tag == NODE_HEAD);

  if (node->tag == EMPTY)
    return 0;

  LLNode* llnode = &node->head;
  while (llnode != NULL) {
    if (llnode->value == value)
      return 1;
    llnode = llnode->next;
  }

  return 0;
}

static void AppendChain(ChainedMapNode* node, arena_ptr_t owner, int value) {
  assert(node->tag == EMPTY || node->tag == NODE_HEAD);

  if (node->tag == EMPTY) {
    node->tag = NODE_HEAD;
    node->head = (LLNode){.next = NULL, .value = value};
    node->lst = &node->head;
    return;
  }

  LLNode* new = (LLNode*)arena_alloc(owner, sizeof(LLNode));

  new->next = NULL;
  new->value = value;

  node->lst->next = new;
  node->lst = new;
}

size_t IntegerSetHash(IntegerSet* set, int value) {
  return (size_t)(value % set->size);
}

IntegerSet* CreateSet(int* arr, int sz) {
  assert(sz >= 0);

  arena_ptr_t owner =
      arena_make(3 * sz * sizeof(ChainedMapNode) + sizeof(IntegerSet),
                 alloc_default, error_arena);

  IntegerSet* set = (IntegerSet*)arena_alloc(owner, sizeof(IntegerSet));

  ChainedMapNode* data = sz == 0 ? NULL
                                 : (ChainedMapNode*)arena_alloc(
                                       owner, sz * sizeof(ChainedMapNode));

  for (size_t i = 0; i < sz; i++) {
    data[i] = (ChainedMapNode){.tag = EMPTY};
  }

  set->owner = owner;
  set->data = data;
  set->size = sz;

  for (size_t i = 0; i < sz; i++) {
    size_t hash = IntegerSetHash(set, arr[i]);
    if (IsInChain(set->data + hash, arr[i])) {
      DeleteSet(set);
      return NULL;
    }
    AppendChain(set->data + hash, set->owner, arr[i]);
  }

  return set;
}

int IsInSet(IntegerSet* set, int value) {
  if (set == NULL || set->size == 0)
    return 0;

  size_t hash = IntegerSetHash(set, value);

  return IsInChain(set->data + hash, value);
}

void DeleteSet(IntegerSet* set) {
  if (set == NULL)
    return;
  arena_release(set->owner);
}
