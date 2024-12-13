#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STRVAL_SIZE 8

#define VECTOR_INIT_SIZE 16
#define VECTOR_GROW_FACTOR 2

typedef struct __attribute__((aligned(8))) {
  size_t capacity;
  size_t length;
} vector_header_t;

typedef vector_header_t* vector_ptr_t;

size_t vector_get_length(vector_ptr_t vec);

size_t vector_get_capacity(vector_ptr_t vec);

void vector_init(vector_ptr_t vec);

void* vector_basepointer(vector_ptr_t vec);

#define vector_make(T) vector_make_sized(sizeof(T))

void vector_free(vector_ptr_t vec);

vector_ptr_t vector_make_sized(size_t elem_size);

vector_ptr_t vector_grow_sized(vector_ptr_t vec, size_t elem_size);

vector_ptr_t vector_append_fake(vector_ptr_t vec, size_t elem_size);

#define vector_grow(T, vec) vector_grow_sized(vec, sizeof(T))

#define vector_append(T, vec, elem)                                \
  ({                                                               \
    vector_ptr_t _grown = vector_append_fake(vec, sizeof(T));      \
    if (_grown != NULL) {                                          \
      ((T*)vector_basepointer(_grown))[_grown->length - 1] = elem; \
    }                                                              \
    _grown;                                                        \
  })

#define vector_get_array(T, vec) (T*)vector_basepointer(vec)

#define vector_get(T, vec, ind) (vector_get_array(T, vec))[ind]

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

bool vector_isempty(vector_ptr_t vec) {
  return vector_get_length(vec) == 0;
}

typedef struct {
  uint64_t key;
  char value[STRVAL_SIZE];
} item_t;

int main() {
  size_t n;

  return 0;
}
