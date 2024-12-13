#include <assert.h>
#include <stdlib.h>
#include "vector.h"

vector_ptr_t vector_make_sized(size_t elem_size) {
  vector_ptr_t mem = (vector_ptr_t)malloc(sizeof(vector_header_t) +
                                          VECTOR_INIT_SIZE * elem_size);
  if (mem == NULL)
    return NULL;
  vector_init(mem);

  return mem;
}

vector_ptr_t vector_grow_sized(vector_ptr_t vec, size_t elem_size) {
  size_t new_cap = vec->capacity * VECTOR_GROW_FACTOR;
  assert(vec->capacity * elem_size < new_cap * elem_size);
  vector_ptr_t new_mem = (vector_ptr_t)realloc(vec, new_cap * elem_size);
  if (new_mem == NULL)
    return NULL;
  new_mem->capacity = new_cap;

  return new_mem;
}

vector_ptr_t vector_append_fake(vector_ptr_t vec, size_t elem_size) {
  if (vec->capacity <= vec->length) {
    vec = vector_grow_sized(vec, elem_size);
    if (vec == NULL)
      return NULL;
  }

  vec->length++;

  return vec;
}

size_t vector_get_length(vector_ptr_t vec) {
  return vec->length;
}

size_t vector_get_capacity(vector_ptr_t vec) {
  return vec->capacity;
}

void vector_init(vector_ptr_t vec) {
  vec->capacity = VECTOR_INIT_SIZE;
  vec->length = 0;
}

void* vector_basepointer(vector_ptr_t vec) {
  return (void*)(vec + 1);
}

void vector_free(vector_ptr_t vec) {
  free(vec);
}
