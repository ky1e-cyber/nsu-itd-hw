#if !defined(H_INTEGERSET)
#define H_INTEGERSET

#include "base.h"

typedef struct LLNode__ {
  struct LLNode__* next;
  int value;
} LLNode;

typedef struct {
  enum { EMPTY, NODE_HEAD } tag;
  LLNode head;
  LLNode* lst;
} ChainedMapNode;

typedef struct {
  arena_ptr_t owner;
  size_t size;
  ChainedMapNode* data;
} IntegerSet;

IntegerSet* CreateSet(int* arr, int sz);

int IsInSet(IntegerSet* set, int value);

void DeleteSet(IntegerSet* set);

#endif
