// BASE START

#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>

// defs

#define WORDSIZE (sizeof(size_t))
#define MAX_ALIGNMENT (alignof(max_align_t))

#if !defined(VECTOR_INIT_CAPACITY)
#define VECTOR_INIT_CAPACITY 16
#endif

#if !defined(VECTOR_GROW_FACTOR)
#define VECTOR_GROW_FACTOR 2
#endif

// macros

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
  static inline __attribute__((always_inline)) __attribute__((unused))

#define m_macro_like_const static const __attribute__((unused))

#define m_contains(elem, arr, sz)        \
  ({                                     \
    const __auto_type elem__ = elem;     \
    const __typeof(elem__)* arr__ = arr; \
    bool f = false;                      \
    for (size_t i = 0; i < sz; i++) {    \
      if (elem__ == arr__[i]) {          \
        f = true;                        \
        break;                           \
      }                                  \
    }                                    \
    f;                                   \
  })

#define m_unreachable __builtin_unreachable()

// error

noreturn void error(char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

// alloc

typedef struct alloc_t_ {
  void* (*const acquire)(size_t);
  void* (*const resize)(void*, size_t);
  void (*const release)(void*);
} alloc_t;

void* acquire_default(size_t size) {
  return malloc(size);
}

void* resize_default(void* mem, size_t new_size) {
  return realloc(mem, new_size);
}

void release_default(void* mem) {
  free(mem);
}

alloc_t alloc_default = {.acquire = acquire_default,
                         .resize = resize_default,
                         .release = release_default};

// memory

m_macro_like size_t mem_align_by(size_t x, size_t alignment) {
  return x + (-x & (alignment - 1));
}

// arena

typedef struct __attribute__((aligned(MAX_ALIGNMENT))) {
  const alloc_t allocator;
  void (*fail_callback)(void);
  size_t size;
  size_t pos;
} arena_header_t;

typedef arena_header_t* arena_ptr_t;

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

m_macro_like void* arena_baseptr(arena_ptr_t arena) {
  return (void*)(arena + 1);
}

m_macro_like const alloc_t* arena_get_allocator(arena_ptr_t arena) {
  return &arena->allocator;
}

void arena_release(arena_ptr_t arena) {
  arena_get_allocator(arena)->release(arena);
}

m_macro_like void arena_cleanup(arena_ptr_t* arenaptr) {
  arena_release(*arenaptr);
}

m_macro_like bool arena_fits(arena_ptr_t arena, size_t sz) {
  return (arena->size - arena->pos) >= sz;
}

void* arena_alloc(arena_ptr_t arena, size_t sz) {
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

// array

typedef struct __attribute__((aligned(MAX_ALIGNMENT))) {
  size_t capacity;
  size_t size;
} array_header_t;

typedef array_header_t* array_ptr_t;

m_macro_like size_t array_bytesize_sized(size_t elem_sz, size_t cnt) {
  return sizeof(array_header_t) + elem_sz * cnt;
}

#define arary_bytesize(T, cnt) /* -> size_t */ \
  array_bytesize_sized(sizeof(T), cnt)

void array_init(array_ptr_t arr, size_t capacity) {
  arr->capacity = capacity;
  arr->size = 0;
}

m_macro_like size_t array_size(array_ptr_t arr) {
  return arr->size;
}

m_macro_like size_t array_capacity(array_ptr_t arr) {
  return arr->capacity;
}

m_macro_like bool array_empty(array_ptr_t arr) {
  return arr->size == 0;
}

m_macro_like void* array_baseptr(array_ptr_t arr) {
  return (void*)(arr + 1);
}

#define array_data(T, arr) /* -> T* */ ((T*)(array_baseptr(arr)))

#define array_fill_from(T, arr, buf, buf_sz) /* -> array_ptr_t */ \
  ({                                                              \
    size_t buf_sz__ = buf_sz;                                     \
    __auto_type buf__ = buf;                                      \
    array_ptr_t arr__ = arr;                                      \
    static_assert(m_types_compatible(T*, &buf__[0]),              \
                  "Buffer type incompatible with elements");      \
    assert(arr__->capacity >= buf_sz);                            \
    if (arr != NULL) {                                            \
      for (size_t i = 0; i < sz; i++)                             \
        array_data(T, arr__)[i] = buf__[i];                       \
    }                                                             \
    arr;                                                          \
  })

#define array_at(T, arr, ind) (array_data(T, arr)[ind])

// vector

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
  vector_bytesize_sized(sizeof(T), cnt)

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

#define vector_make(T, alloc, fail_callback) /* -> vector_ptr_t */ \
  vector_make_sized(sizeof(T), alloc, fail_callback)

m_macro_like size_t vector_size(vector_ptr_t vec) {
  return vec->size;
}

m_macro_like size_t vector_capacity(vector_ptr_t vec) {
  return vec->capacity;
}

m_macro_like bool vector_empty(vector_ptr_t vec) {
  return vec->size == 0;
}

m_macro_like void* vector_baseptr(vector_ptr_t vec) {
  return (void*)(vec + 1);
}

m_macro_like alloc_t vector_get_allocator(vector_ptr_t vec) {
  return vec->allocator;
}

void vector_release(vector_ptr_t vec) {
  vector_get_allocator(vec).release(vec);
}

m_macro_like void vector_cleanup(vector_ptr_t* vecptr) {
  vector_release(*vecptr);
}
#define vector_data(T, vec) /* -> T* */ ((T*)(vector_baseptr(vec)))

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

#define vector_grow(T, vec) /* -> vector_ptr_t */ \
  vector_grow_sized(vec, sizeof(T))

#define vector_at(T, vec, ind) /* -> T */ (vector_data(T, vec)[ind])

#define vector_push_back(T, vec, elem) /* -> vector_ptr_t */              \
  ({                                                                      \
    vector_ptr_t vec__ = vec;                                             \
    vector_ptr_t grown__ = (vector_size(vec__) == vector_capacity(vec__)) \
                               ? vector_grow(T, vec__)                    \
                               : vec__;                                   \
    if (grown__ != NULL) {                                                \
      (vector_data(T, grown__))[(grown__->size)++] = (T)elem;             \
    }                                                                     \
    grown__;                                                              \
  })

m_macro_like void vector_remove_back(vector_ptr_t vec) {
  vec->size--;
}

#define vector_pop_back(T, vec) /* -> T */                 \
  ({                                                       \
    vector_ptr_t vec__ = vec;                              \
    T res__ = vector_at(T, vec__, vector_size(vec__) - 1); \
    vector_remove_back(vec__);                             \
    res__;                                                 \
  })

#define vector_begin(T, vec) /* -> T* */ vector_data(T, vec)

#define vector_end(T, vec) /* -> T* */ \
  ({                                   \
    vector_ptr_t vec__ = vec;          \
    T* data__ = vector_data(T, vec__); \
    data__ + vector_size(vec__);       \
  })

#define vector_foreach(T, vec, op)                           \
  {                                                          \
    void (*op__)(T) = op;                                    \
    vector_ptr_t vec__ = vec;                                \
    T* end__ = vector_end(T, vec__);                         \
    for (T* it = vector_begin(T, vec__); it < end__; it++) { \
      op__(it);                                              \
    }                                                        \
  }

m_macro_like void vector_clear(vector_ptr_t vec) {
  vec->size = 0;
}

// BASE END

#include <assert.h>
#include <ctype.h>
#include <stdint.h>

typedef enum { OP_PLUS, OP_MINUS, OP_MUL, OP_DIV } op_t;

typedef struct {
  enum { TK_VALUE = 0, TK_OP, TK_PAREN_O, TK_PAREN_C, TK_EOS } kind;
  union {
    uint64_t as_u64;
    op_t as_op;
  } value;
} token_t;

typedef struct expr_t__ expr_t;

typedef struct {
  op_t op;
  expr_t* lhs;
  expr_t* rhs;
} expr_binop_t;

struct expr_t__ {
  enum { EXPR_VALUE, EXPR_OP } kind;
  union {
    expr_binop_t as_binop;
    double as_f64;
  } expr;
};

typedef struct {
  uint8_t left;
  uint8_t right;
} uint8_pair_t;

static expr_t* expr_init_binop(expr_t* e, op_t op, expr_t* lhs, expr_t* rhs) {
  e->kind = EXPR_OP;
  e->expr.as_binop = (expr_binop_t){.op = op, .lhs = lhs, .rhs = rhs};
  return e;
}

static expr_t* expr_init_value(expr_t* e, double value) {
  e->kind = EXPR_VALUE;
  e->expr.as_f64 = value;
  return e;
}

m_macro_like uint8_pair_t uint8_pair(uint8_t left, uint8_t right) {
  return (uint8_pair_t){.left = left, .right = right};
}

static uint8_pair_t binding_power(op_t op) {
  switch (op) {
    case OP_MINUS:
    case OP_PLUS:
      return uint8_pair(1, 2);
    case OP_MUL:
    case OP_DIV:
      return uint8_pair(3, 4);
  }

  m_unreachable;
}

static noreturn void lexer_error__() {
  error("Lexer error\n");
}

static struct {
  FILE* stream;
  int current_char;
  token_t current_token;
  bool eof;
} g_lexer_state;

static void lexer_init(FILE* stream) {
  g_lexer_state.stream = stream;
  g_lexer_state.current_char = getc(stream);
  g_lexer_state.eof = g_lexer_state.current_char == EOF;
  g_lexer_state.current_token = g_lexer_state.eof
                                    ? (token_t){.kind = TK_EOS}
                                    : (token_t){.kind = TK_PAREN_O};
}

static int lexer_peek_char() {
  return g_lexer_state.current_char;
}

static int lexer_consume_char() {
  int c = lexer_peek_char();

  if (c == EOF) {
    g_lexer_state.eof = true;
  } else {
    int nxt = getc(g_lexer_state.stream);
    g_lexer_state.current_char = nxt;
  }

  return c;
}

m_macro_like bool is_whitespace(int c) {
  return (c == ' ' || c == '\t' || c == '\n');
}

static token_t lexer_tokenize_int() {
  vector_ptr_t buf =
      vector_make(char, alloc_default, lexer_error__);

  int c = lexer_peek_char();

  while (isdigit(c)) {
    buf = vector_push_back(char, buf, (char)lexer_consume_char());
    c = lexer_peek_char();
  }

  buf = vector_push_back(char, buf, '\0');

  char* end__;
  uint64_t val = (uint64_t)strtoull(vector_data(char, buf), &end__, 10);

  vector_release(buf);
  return (token_t){.kind = TK_VALUE, .value.as_u64 = val};
}

static token_t lexer_next_token() {
  assert(!g_lexer_state.eof &&
         "lexer_consume_token called after EOF encountered");

  const int ops_set[] = {'+', '-', '*', '/'};
  const op_t ops_map[] = {
      ['+'] = OP_PLUS, ['-'] = OP_MINUS, ['*'] = OP_MUL, ['/'] = OP_DIV};

  int c = lexer_peek_char();
  while (is_whitespace(c)) {
    lexer_consume_char();
    c = lexer_peek_char();
  }

  if (c == '(') {
    lexer_consume_char();
    return (token_t){.kind = TK_PAREN_O};
  }

  if (c == ')') {
    lexer_consume_char();
    return (token_t){.kind = TK_PAREN_C};
  }

  if (m_contains(c, ops_set, sizeof(ops_set) / sizeof(int))) {
    lexer_consume_char();
    return (token_t){.kind = TK_OP, .value.as_op = ops_map[c]};
  }

  if (isdigit(c))
    return lexer_tokenize_int();

  c = lexer_consume_char();
  if (c == EOF)
    return (token_t){.kind = TK_EOS};

  error("Invalid char encountered\n");
}

static vector_ptr_t tokenize(FILE* inp) {
  lexer_init(inp);
  vector_ptr_t res = vector_make(token_t, alloc_default, lexer_error__);

  token_t tk = lexer_next_token();

  while (tk.kind != TK_EOS) {
    res = vector_push_back(token_t, res, tk);
    tk = lexer_next_token();
  }

  res = vector_push_back(token_t, res, tk);

  return res;
}

struct {
  vector_ptr_t tokens;
  size_t tokens_pos;
} g_parser_state;

static void parser_init(FILE* stream) {
  vector_ptr_t tokens = tokenize(stream);

  g_parser_state.tokens = tokens;
  g_parser_state.tokens_pos = 0;
}

static void parser_deinit() {
  vector_release(g_parser_state.tokens);
}

static token_t parser_peek_token() {
  return vector_at(token_t, g_parser_state.tokens, g_parser_state.tokens_pos);
}

static token_t parser_consume_token() {
  token_t res = parser_peek_token();
  g_parser_state.tokens_pos++;
  return res;
}

static expr_t* parse_pratt_bp(arena_ptr_t expr_arena, uint8_t min_bp);
static expr_t* parse_toplevel(arena_ptr_t expr_arena);

static expr_t* parse_pratt_bp(arena_ptr_t expr_arena, uint8_t min_bp) {
  bool negate = false;

  token_t tk = parser_peek_token();
  if (tk.kind == TK_OP && tk.value.as_op == OP_MINUS) {
    negate = true;
    parser_consume_token();
    tk = parser_peek_token();
  }

  expr_t* lhs;

  if (tk.kind == TK_PAREN_O) {
    parser_consume_token();
    lhs = parse_pratt_bp(expr_arena, 0);

    token_t paren = parser_consume_token();
    if (paren.kind != TK_PAREN_C)
      error("Parser error: non-closed grouping encountered\n");
  
    if (negate) {
      expr_t* zero = expr_init_value(
          (expr_t*)arena_alloc(expr_arena, sizeof(expr_t)), 0.0);

      lhs = expr_init_binop((expr_t*)arena_alloc(expr_arena, sizeof(expr_t)),
                            OP_MINUS, zero, lhs);
    }
  } else if (tk.kind == TK_VALUE) {
    lhs = expr_init_value((expr_t*)arena_alloc(expr_arena, sizeof(expr_t)),
                          ((double)tk.value.as_u64) * (negate ? -1.0 : 1.0));
    parser_consume_token();
  }

  while (true) {
    token_t tk = parser_peek_token();

    if (tk.kind == TK_PAREN_C || tk.kind == TK_EOS)
      break;

    if (tk.kind != TK_OP)
      error("Parser error: non-op token in unexpected place\n");

    op_t op = tk.value.as_op;
    uint8_pair_t bp = binding_power(op);

    if (bp.left < min_bp)
      break;

    parser_consume_token();

    expr_t* rhs = parse_pratt_bp(expr_arena, bp.right);

    expr_t* new_lhs = expr_init_binop(
        (expr_t*)arena_alloc(expr_arena, sizeof(expr_t)), op, lhs, rhs);

    lhs = new_lhs;
  }

  return lhs;
}

static expr_t* parse_toplevel(arena_ptr_t expr_arena) {
  expr_t* e = parse_pratt_bp(expr_arena, 0);

  if (parser_peek_token().kind != TK_EOS)
    error("Parser error: expression wasn't parsed?\n");

  return e;
}

expr_t* parse(FILE* inp, arena_ptr_t expr_arena) {
  parser_init(inp);
  expr_t* e = parse_toplevel(expr_arena);
  parser_deinit();

  return e;
}

static noreturn void parser_error__() {
  error("Parser error\n");
}

static double eval_expr(expr_t* e) {
  switch (e->kind) {
    case EXPR_VALUE:
      return e->expr.as_f64;
    case EXPR_OP:;
      double lhs = eval_expr(e->expr.as_binop.lhs);
      double rhs = eval_expr(e->expr.as_binop.rhs);
      switch (e->expr.as_binop.op) {
        case OP_PLUS:
          return lhs + rhs;
        case OP_MINUS:
          return lhs - rhs;
        case OP_MUL:
          return lhs * rhs;
        case OP_DIV:
          return lhs / rhs;
      }
  }

  error("Eval error\n");
}

int main() {
  arena_ptr_t expr_arena =
      arena_make(1 << 25, alloc_default, parser_error__);

  expr_t* e = parse(stdin, expr_arena);
  printf("%.20lf\n", eval_expr(e));
  
  arena_release(expr_arena);

  return 0;
}
