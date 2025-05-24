#if !defined(H_BASE)
#define H_BASE

#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>

#define WORDSIZE (sizeof(size_t))
#define MAX_ALIGNMENT (alignof(max_align_t))

#if !defined(VECTOR_INIT_CAPACITY)
#define VECTOR_INIT_CAPACITY 16
#endif

#if !defined(VECTOR_GROW_FACTOR)
#define VECTOR_GROW_FACTOR 2
#endif

#define m_id__(x, y) x##y

#define m_id(x, y) m_id__(x, y)

#define m_unique(x) m_id(x, __COUNTER__)

#define m_abs(x) ({ const __auto_type x_ = x; })

#define m_sign(x) ((x) < 0 ? -1 : 1)

#define m_min(x, y)             \
  ({                            \
    const __auto_type x_ = (x); \
    const __auto_type y_ = (y); \
    x_ < y_ ? x_ : y_;          \
  })

#define m_max(x, y)             \
  ({                            \
    const __auto_type x_ = (x); \
    const __auto_type y_ = (y); \
    x_ > y_ ? x_ : y_;          \
  })

#define m_types_compatible(T1, T2) __builtin_types_compatible_p(T1, T2)

#define m_assert_istype(T) \
  static_assert(m_types_compatible(T, T), "Type expected")

#define m_isarray(a) (!m_types_compatible(typeof(a), typeof(&a[0])))

#define m_cleanup(f) __attribute__((cleanup(f)))

#define m_defer__(v__, f) int v__ __attribute__((__cleanup__(f)))

#define m_defer(f) m_defer__(m_unique(defer_v), f)

#define m_op_overflow__(x, y, checkop)   \
  ({                                     \
    const __auto_type x_ = (x);          \
    const __auto_type y_ = (y);          \
    checkop(x_, y_, (typeof(x_ + y_))0); \
  })

#define m_add_overflow(x, y) m_op_overflow__(x, y, __builtin_add_overflow_p)

#define m_sub_overflow(x, y) m_op_overflow__(x, y, __builtin_sub_overflow_p)

#define m_mul_overflow(x, y) m_op_overflow__(x, y, __builtin_mul_overflow_p)

#define m_macro_like \
  [[maybe_unused]] static inline __attribute__((always_inline))

#define m_macro_like_const [[maybe_unused]] static const

#define m_contains(elem, arr, sz)      \
  ({                                   \
    const __auto_type elem__ = elem;   \
    const typeof(elem__)* arr__ = arr; \
    bool f = false;                    \
    for (size_t i = 0; i < sz; i++) {  \
      if (elem__ == arr__[i]) {        \
        f = true;                      \
        break;                         \
      }                                \
    }                                  \
    f;                                 \
  })

#define m_unreachable __builtin_unreachable()

static noreturn void error(char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

m_macro_like size_t mem_align_by(size_t x, size_t alignment) {
  return x + (-x & (alignment - 1));
}

typedef struct alloc_t_ {
  void* (*const acquire)(size_t);
  void* (*const resize)(void*, size_t);
  void (*const release)(void*);
} alloc_t;

static void* acquire_default(size_t size) {
  return malloc(size);
}

static void* resize_default(void* mem, size_t new_size) {
  return realloc(mem, new_size);
}

static void release_default(void* mem) {
  free(mem);
}

static alloc_t alloc_default = {.acquire = acquire_default,
                                .resize = resize_default,
                                .release = release_default};

typedef struct __attribute__((aligned(MAX_ALIGNMENT))) {
  const alloc_t allocator;
  void (*fail_callback)(void);
  size_t size;
  size_t pos;
} arena_header_t;

typedef arena_header_t* arena_ptr_t;

arena_ptr_t arena_make(size_t sz, alloc_t alloc, void (*fail_callback)(void));

m_macro_like const alloc_t* arena_get_allocator(arena_ptr_t arena) {
  return &arena->allocator;
}

static void arena_release(arena_ptr_t arena) {
  arena_get_allocator(arena)->release(arena);
}

m_macro_like void arena_cleanup(arena_ptr_t* arenaptr) {
  arena_release(*arenaptr);
}

m_macro_like void* arena_baseptr(arena_ptr_t arena) {
  return (void*)(arena + 1);
}

m_macro_like bool arena_fits(arena_ptr_t arena, size_t sz) {
  return (arena->size - arena->pos) >= sz;
}

arena_ptr_t arena_make(size_t sz, alloc_t alloc, void (*fail_callback)(void)) {
  arena_ptr_t arena = (arena_ptr_t)alloc.acquire(sizeof(arena_header_t) + sz);

  if (arena == NULL) {
    fail_callback();
    return NULL;
  }

  arena_header_t header = {
      .allocator = alloc, .fail_callback = fail_callback, .size = sz, .pos = 0};
  memcpy(arena, &header, sizeof(arena_header_t));

  return arena;
}

static void* arena_alloc(arena_ptr_t arena, size_t sz) {
  if (!arena_fits(arena, sz)) {
    arena->fail_callback();
    return NULL;
  }

  void* mem = arena_baseptr(arena) + arena->pos;

  size_t padded = mem_align_by(sz, MAX_ALIGNMENT);

  arena->pos +=
      arena->pos + padded > arena->size ? arena->size - arena->pos : padded;

  return mem;
}

typedef struct __attribute__((aligned(MAX_ALIGNMENT))) {
  const size_t size;
} array_header_t;

typedef array_header_t* array_ptr_t;

m_macro_like size_t array_bytesize_sized(size_t elem_sz, size_t cnt) {
  return sizeof(array_header_t) + elem_sz * cnt;
}

#define array_bytesize(T, cnt) /* -> size_t */ \
  ({                                           \
    m_assert_istype(T);                        \
    size_t cnt__ = cnt;                        \
    array_bytesize_sized(sizeof(T), cnt__);    \
  })

m_macro_like array_ptr_t array_init(array_ptr_t arr, size_t sz) {
  array_header_t arr_hd = {.size = sz};
  memcpy(arr, &arr_hd, sizeof(array_header_t));
  return arr;
}

m_macro_like size_t array_size(array_ptr_t arr) {
  return arr->size;
}

m_macro_like void* array_baseptr(array_ptr_t arr) {
  return (void*)(arr + 1);
}

#define array_data(T, arr) /* -> T* */ \
  ({                                   \
    m_assert_istype(T);                \
    array_ptr_t arr__ = arr;           \
    (T*)array_baseptr(arr__);          \
  })

#define array_begin(T, arr) /* -> T* */ \
  ({                                    \
    m_assert_istype(T);                 \
    array_ptr_t arr__ = arr;            \
    (T*)array_baseptr(arr__);           \
  })

#define array_end(T, arr) /* -> T* */     \
  ({                                      \
    m_assert_istype(T);                   \
    array_ptr_t arr__ = arr;              \
    T* data__ = (T*)array_baseptr(arr__); \
    data__ + array_size(arr__);           \
  })

#define array_fill_from(T, arr, buf, buf_sz) /* -> void */ \
  {                                                        \
    m_assert_istype(T);                                    \
    size_t buf_sz__ = buf_sz;                              \
    T* buf__ = buf;                                        \
    array_ptr_t arr__ = arr;                               \
    assert(arr__ != NULL);                                 \
    assert(array_size(arr__) >= buf_sz__);                 \
    for (size_t i = 0; i < array_size(arr__); i++)         \
      ((T*)array_baseptr(arr__))[i] = buf__[i];            \
  }

#define array_foreach(T, arr, op) /* -> void */                \
  {                                                            \
    m_assert_istype(T);                                        \
    array_ptr_t arr__ = arr;                                   \
    void (*op__)(T*) = op;                                     \
    T* begin__ = ((T*)array_baseptr(arr__));                   \
    T* end__ = begin__ + array_size(arr__);                    \
    for (T* it = (T*)array_baseptr(arr__); it < end__; it++) { \
      op__(it);                                                \
    }                                                          \
  }

typedef struct __attribute__((aligned(MAX_ALIGNMENT))) {
  const alloc_t allocator;
  void (*fail_callback)(void);
  size_t capacity;
  size_t size;
} vector_header_t;

typedef vector_header_t* vector_ptr_t;

m_macro_like size_t vector_bytesize_sized(size_t elem_sz, size_t cnt) {
  return sizeof(vector_header_t) + elem_sz * cnt;
}

#define vector_bytesize(T, cnt) /* -> size_t */ \
  ({                                            \
    m_assert_istype(T);                         \
    size_t cnt__ = cnt;                         \
    vector_bytesize_sized(sizeof(T), cnt__);    \
  })

#define vector_make(T, alloc, fail_callback) /* -> vector_ptr_t */ \
  ({                                                               \
    m_assert_istype(T);                                            \
    alloc_t alloc__ = alloc;                                       \
    vector_make_sized(sizeof(T), alloc__, fail_callback);          \
  })

m_macro_like alloc_t vector_get_allocator(vector_ptr_t vec) {
  return vec->allocator;
}

void vector_release(vector_ptr_t vec) {
  vector_get_allocator(vec).release(vec);
}

m_macro_like void vector_cleanup(vector_ptr_t* vecptr) {
  vector_release(*vecptr);
}

m_macro_like size_t vector_size(vector_ptr_t vec) {
  return vec->size;
}

m_macro_like size_t vector_capacity(vector_ptr_t vec) {
  return vec->capacity;
}

m_macro_like void* vector_baseptr(vector_ptr_t vec) {
  return (void*)(vec + 1);
}

#define vector_data(T, vec) /* -> T* */ \
  ({                                    \
    m_assert_istype(T);                 \
    vector_ptr_t vec__ = vec;           \
    (T*)vector_baseptr(vec__);          \
  })

#define vector_grow(T, vec) /* -> vector_ptr_t */ \
  ({                                              \
    m_assert_istype(T);                           \
    vector_grow_sized(vec, sizeof(T));            \
  })

#define vector_push_back(T, vec, elem) /* -> vector_ptr_t */              \
  ({                                                                      \
    m_assert_istype(T);                                                   \
    T elem__ = elem;                                                      \
    vector_ptr_t vec__ = vec;                                             \
    vector_ptr_t grown__ = (vector_size(vec__) == vector_capacity(vec__)) \
                               ? vector_grow_sized(vec__, sizeof(T))      \
                               : vec__;                                   \
    if (grown__ != NULL) {                                                \
      ((T*)vector_baseptr(grown__))[(grown__->size)++] = (T)elem__;       \
    }                                                                     \
    grown__;                                                              \
  })

m_macro_like void vector_remove_back(vector_ptr_t vec) {
  vec->size--;
}

#define vector_pop_back(T, vec) /* -> T */                         \
  ({                                                               \
    m_assert_istype(T);                                            \
    vector_ptr_t vec__ = vec;                                      \
    T res__ = ((T*)vector_baseptr(vec__))[vector_size(vec__) - 1]; \
    vector_remove_back(vec__);                                     \
    res__;                                                         \
  })

#define vector_begin(T, vec) /* -> T* */ \
  ({                                     \
    m_assert_istype(T);                  \
    vector_ptr_t vec__ = vec;            \
    (T*)vector_baseptr(vec__);           \
  })

#define vector_end(T, vec) /* -> T* */                \
  ({                                                  \
    m_assert_istype(T);                               \
    vector_ptr_t vec__ = vec;                         \
    ((T*)vector_baseptr(vec__)) + vector_size(vec__); \
  })

#define vector_foreach(T, vec, op) /* -> void */ \
  {                                              \
    m_assert_istype(T);                          \
    vector_ptr_t vec__ = vec;                    \
    void (*op__)(T*) = op;                       \
    T* begin__ = (T*)vector_baseptr(vec__);      \
    T* end__ = begin__ + vector_size(vec__);     \
    for (T* it = begin__; it < end__; it++) {    \
      op__(it);                                  \
    }                                            \
  }

m_macro_like void vector_clear(vector_ptr_t vec) {
  vec->size = 0;
}

vector_ptr_t vector_make_sized(size_t elem_sz,
                               alloc_t alloc,
                               void (*fail_callback)(void)) {
  vector_ptr_t vec = (vector_ptr_t)alloc.acquire(
      vector_bytesize_sized(elem_sz, VECTOR_INIT_CAPACITY));
  if (vec == NULL) {
    fail_callback();
    return NULL;
  }

  vector_header_t vec_hd = {
      .allocator = alloc, .capacity = VECTOR_INIT_CAPACITY, .size = 0};
  memcpy(vec, &vec_hd, sizeof(vector_header_t));

  return vec;
}

vector_ptr_t vector_grow_sized(vector_ptr_t vec, size_t elem_sz) {
  const size_t old_cap = vector_capacity(vec);
  const size_t new_cap = old_cap * VECTOR_GROW_FACTOR;
  const alloc_t alloc = vector_get_allocator(vec);
  vector_ptr_t new_mem =
      new_cap <= old_cap  // relies on -fwrapv
          ? NULL
          : (vector_ptr_t)(alloc.resize(
                vec, vector_bytesize_sized(elem_sz, new_cap)));
  if (new_mem == NULL) {
    vec->fail_callback();
  } else {
    new_mem->capacity = new_cap;
  }

  return new_mem;
}

#endif
