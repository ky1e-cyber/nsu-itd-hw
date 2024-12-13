#if !defined(H_VECTOR)
#define H_VECTOR

#include <assert.h>
#include <stdlib.h>

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

#endif
