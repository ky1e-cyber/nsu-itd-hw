#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// static inline void swap(int32_t* a, int32_t* b) {
//   int32_t t = *a;
//   *a = *b;
//   *b = t;
// }

typedef uint8_t byte;

typedef struct __attribute__((packed, aligned(8))) {
  size_t size;  // size of arena
  size_t pos;   // (aligned) bytes allocated
} arena_header_t;

typedef arena_header_t* aptr_t;

static inline byte* arena_basepointer(aptr_t arena) {
  return (byte*)(arena + 1);
}

static inline bool arena_isfits(aptr_t arena, size_t size) {
  size_t free_bytes = arena->size - arena->pos;
  return size <= free_bytes;
}

aptr_t arena_make(size_t size) {
  void* mem = malloc(sizeof(arena_header_t) + size);
  if (mem == NULL) {
    return (aptr_t)NULL;
  }

  aptr_t arena = (aptr_t)mem;
  arena->pos = 0;
  arena->size = size;

  return arena;
}

void* arena_alloc(aptr_t arena, size_t size) {
  size_t aligned_size = size;
  while (aligned_size % 8 != 0)
    aligned_size++;

  if (!arena_isfits(arena, aligned_size)) {
    return (void*)NULL;
  }

  void* mem = (void*)(arena_basepointer(arena) + (arena->pos));
  arena->pos = arena->pos + aligned_size;

  return mem;
}

void arena_free(aptr_t arena) {
  free((void*)arena);
}

typedef struct {
  int32_t value;
  size_t count;
} item_t;

typedef struct _bstree_t {
  item_t item;
  struct _bstree_t* left;
  struct _bstree_t* right;
} bstree_t;

static inline bstree_t* bstree_new(int32_t val, aptr_t arena) {
  bstree_t* new = (bstree_t*)arena_alloc(arena, sizeof(bstree_t));
  if (new == NULL)
    return NULL;
  *new = (bstree_t){
      .item = {.count = 1, .value = val}, .left = NULL, .right = NULL};

  return new;
}

static bstree_t* bstree_insert_tail(bstree_t* tree,
                                    bstree_t** dst,
                                    bstree_t* ret,
                                    int32_t val,
                                    aptr_t arena) {
  if (tree == NULL) {
    bstree_t* new = bstree_new(val, arena);
    if (new == NULL)
      return NULL;
    *dst = new;
    return ret;
  }

  if (tree->item.value == val) {
    tree->item.count++;
    return ret;
  }

  return (val > tree->item.value)
             ? bstree_insert_tail(tree->right, &tree->right, ret, val, arena)
             : bstree_insert_tail(tree->left, &tree->left, ret, val, arena);
}

bstree_t* bstree_insert(bstree_t* tree, int32_t val, aptr_t arena) {
  if (tree == NULL) {
    bstree_t* new = bstree_new(val, arena);
    if (new == NULL)
      return NULL;
    return new;
  }

  return (val > tree->item.value)
             ? bstree_insert_tail(tree->right, &tree->right, tree, val, arena)
             : bstree_insert_tail(tree->left, &tree->left, tree, val, arena);
}

uint32_t bstree_fill_array(bstree_t* rt, int32_t* arr, uint32_t ind) {
  if (rt == NULL)
    return ind;

  ind = bstree_fill_array(rt->left, arr, ind);
  for (uint32_t i = 0; i < rt->item.count; i++)
    arr[ind++] = rt->item.value;
  ind = bstree_fill_array(rt->right, arr, ind);

  return ind;
}

size_t rand_ind() {
  static const size_t rdiff = SIZE_MAX - RAND_MAX;

  const double mult = (double)rand() / RAND_MAX;

  return (size_t)(rdiff * mult);
}

void shuffle(int32_t* arr, size_t n) {
  if (n > 1) {
    for (size_t i = 0; i < n - 1; i++) {
      size_t j = i + rand_ind() / ((SIZE_MAX) / (n - i) + 1);
      int t = arr[j];
      arr[j] = arr[i];
      arr[i] = t;
    }
  }
}

int main() {
  srand(0x24213);

  FILE* in = fopen("input.bin", "rb");
  FILE* out = fopen("output.bin", "wb");

  uint32_t n;
  fread(&n, 4, 1, in);

  int32_t* xs = (int32_t*)malloc(n * 4);
  fread(xs, 4, n, in);

  shuffle(xs, n);

  static const size_t ARENA_SIZE = 1 << 24;
  aptr_t arena = arena_make(ARENA_SIZE);
  if (arena == NULL)
    return 1;

  bstree_t* tree = NULL;

  for (uint32_t i = 0; i < n; i++) {
    tree = bstree_insert(tree, xs[i], arena);
  }
  bstree_fill_array(tree, xs, 0);

  fwrite(xs, 4, n, out);

  fclose(in);
  fclose(out);

  free(xs);
  arena_free(arena);

  return 0;
}

