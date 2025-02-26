#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static inline void swap(int32_t* a, int32_t* b) {
  int32_t t = *a;
  *a = *b;
  *b = t;
}

typedef struct {
  int32_t* data;
  size_t capacity;
  size_t size;
} bheap_t;

bheap_t* bheap_init(bheap_t* heap, size_t cap) {
  int32_t* dat = (int32_t*)malloc(cap * 4);
  if (dat == NULL)
    return NULL;

  heap->data = dat;
  heap->capacity = cap;
  heap->size = 0;

  return heap;
}

void bheap_free(bheap_t* heap) {
  free(heap->data);
  heap->size = 0;
  heap->capacity = 0;
}

static inline int64_t bheap_get_parrent_ind(size_t ind) {
  if (ind == 0)
    return -1;
  return (size_t)((ind - 1) / 2);
}

static inline int64_t bheap_min_child_ind(bheap_t* heap, size_t ind) {
  size_t left = 2 * ind + 1;
  size_t right = 2 * ind + 2;

  if (heap->size < right)
    return -1;
  if (heap->size == right)
    return left;

  return heap->data[left] < heap->data[right] ? left : right;
}

static void bheap_siftdown(bheap_t* heap, size_t ind) {
  int64_t cur_ind = ind;
  int64_t child_ind = bheap_min_child_ind(heap, ind);

  while ((child_ind != -1) && (heap->data[cur_ind] > heap->data[child_ind])) {
    swap(heap->data + cur_ind, heap->data + child_ind);
    cur_ind = child_ind;
    child_ind = bheap_min_child_ind(heap, cur_ind);
  }
}

static void bheap_siftup(bheap_t* heap, size_t ind) {
  int64_t par_ind = bheap_get_parrent_ind(ind);

  while ((par_ind != -1) && (heap->data[par_ind] > heap->data[ind])) {
    swap(heap->data + par_ind, heap->data + ind);
    ind = par_ind;
    par_ind = bheap_get_parrent_ind(par_ind);
  }
}

void bheap_insert(bheap_t* heap, int32_t val) {
  heap->data[heap->size++] = val;
  bheap_siftup(heap, heap->size - 1);
}

int32_t bheap_pop(bheap_t* heap) {
  int32_t res = heap->data[0];

  if (heap->size == 1) {
    heap->size = 0;
    return res;
  }

  swap(heap->data, heap->data + heap->size - 1);
  heap->size--;
  bheap_siftdown(heap, 0);

  return res;
}

int main() {
  FILE* in = fopen("input.bin", "rb");
  FILE* out = fopen("output.bin", "wb");

  uint32_t n;
  fread(&n, 4, 1, in);

  int32_t* xs = (int32_t*)malloc(n * 4);
  fread(xs, 4, n, in);

  bheap_t heap;
  bheap_t* heapptr = bheap_init(&heap, n);
  if (heapptr == NULL)
    return 1;

  for (uint32_t i = 0; i < n; i++)
    bheap_insert(heapptr, xs[i]);

  for (uint32_t i = 0; i < n; i++)
    xs[i] = bheap_pop(heapptr);

  fwrite(xs, 4, n, out);

  fclose(in);
  fclose(out);

  free(xs);
  bheap_free(heapptr);

  return 0;
}

