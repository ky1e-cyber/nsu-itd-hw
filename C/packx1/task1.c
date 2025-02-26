#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  enum { SOME, NONE } tag;
  int32_t fst;
  int32_t snd;
} optional_pair_t;

static inline bool chr_is_num(const char c) {
  return (c >= '0' && c <= '9');
}

static inline bool str_is_num(const char* str) {
  str = *str == '-' ? str + 1 : str;

  if (*str == '\0')
    return false;

  while (*str != '\0') {
    if (!chr_is_num(*(str++)))
      return false;
  }

  return true;
}

static inline bool is_flag(const char* str) {
  if (*str != '-')
    return false;

  return !chr_is_num(str[1]);
}

const size_t OPERANDS_LIMIT = 3;
const size_t STACK_LIMIT = 2;

typedef struct {
  uint32_t modulo;
} flags_t;

typedef struct {
  char** arr;
  size_t arrsize;
  size_t sp;
} stack_wrapper_t;

// NOP must be always last
typedef enum { ADD = 0, SUB, MUL, NOP } op_t;

static int64_t add_func(int32_t a, int32_t b) {
  return (int64_t)a + b;
}
static int64_t sub_func(int32_t a, int32_t b) {
  return (int64_t)a - b;
}
static int64_t mul_func(int32_t a, int32_t b) {
  return (int64_t)a * b;
}

static int64_t (*const OP_FUNC_MAP[])(int32_t, int32_t) =
    {[ADD] = add_func, [SUB] = sub_func, [MUL] = mul_func};

static const struct {
  char* key;
  op_t op;
} STR_OP_LINMAP[] = {{"add", ADD}, {"sub", SUB}, {"mul", MUL}};

stack_wrapper_t* sw_init(stack_wrapper_t* sw, char** arr, size_t arrsize) {
  if (arr == NULL)
    return NULL;
  sw->arr = arr;
  sw->arrsize = arrsize;
  sw->sp = 0;

  return sw;
}

bool sw_isempty(stack_wrapper_t* sw) {
  return sw->sp == sw->arrsize;
}

char* sw_peek(stack_wrapper_t* sw) {
  if (sw_isempty(sw))
    return NULL;

  return sw->arr[sw->sp];
}

char* sw_pop(stack_wrapper_t* sw) {
  if (sw_isempty(sw))
    return NULL;

  return sw->arr[sw->sp++];
}

flags_t* get_flags(flags_t* flags, stack_wrapper_t* args_stack) {
  flags->modulo = 0;

  char* top = sw_peek(args_stack);

  if (top == NULL)
    return flags;

  if (is_flag(top)) {
    while (is_flag(top)) {
      // can add new flags just by new if (strcmp(top, ...) == 0) case
      if (strcmp(top, "-m") == 0) {
        (void)sw_pop(args_stack);
        char* arg = sw_peek(args_stack);
        if (!str_is_num(arg))
          return NULL;
        (void)sw_pop(args_stack);

        sscanf(arg, "%u", &flags->modulo);
      } else {
        // unknown flag, abort
        return NULL;
      }

      top = sw_peek(args_stack);
      if (top == NULL)
        return flags;
    }
  }

  stack_wrapper_t args_cpy = *args_stack;

  while (!is_flag(top)) {
    sw_pop(&args_cpy);
    top = sw_peek(&args_cpy);
    if (top == NULL)
      return flags;
  }

  args_stack->arrsize =
      args_cpy.sp;  // cut the original args stack where flags start

  return get_flags(flags, &args_cpy);
}

optional_pair_t get_operands(stack_wrapper_t* args_stack) {
  optional_pair_t ret = (optional_pair_t){.tag = NONE};

  int32_t* dst = &ret.fst;
  int32_t* const dst_nxt = &ret.snd;

  for (uint8_t i = 0; i < 2; i++) {
    char* oprnd = sw_pop(args_stack);
    if (oprnd == NULL)
      return ret;

    if (!str_is_num(oprnd))
      return ret;

    sscanf(oprnd, "%u", dst);
    dst = dst_nxt;
  }

  ret.tag = SOME;
  return ret;
}

// i love C types
int64_t (*get_op(stack_wrapper_t* args_stack))(int32_t, int32_t) {
  char* opname = sw_pop(args_stack);
  if (opname == NULL)
    return NULL;

  for (size_t i = 0; i < NOP; i++) {
    if (strcmp(opname, STR_OP_LINMAP[i].key) == 0)
      return OP_FUNC_MAP[STR_OP_LINMAP[i].op];
  }

  return NULL;
}

static int64_t modulo(int64_t a, uint32_t b) {
  int64_t c = a % b;
  return c >= 0 ? c : c + b;
}

int main(int argc, char** argv) {
  static const char* NO_PARAMETERS = "No parameters specified.";

  stack_wrapper_t args_stack;
  assert(sw_init(&args_stack, argv + 1, argc - 1) != NULL);

  flags_t flags = {.modulo = 0};
  if (get_flags(&flags, &args_stack) == NULL) {
    fputs(NO_PARAMETERS, stderr);
    return 13;
  }

  int64_t (*op)(int32_t, int32_t) = get_op(&args_stack);
  if (op == NULL) {
    fputs(NO_PARAMETERS, stderr);
    return 13;
  }

  optional_pair_t oprnds = get_operands(&args_stack);
  if (oprnds.tag == NONE) {
    fputs(NO_PARAMETERS, stderr);
    return 13;
  }

  long long int res = (long long int)op(oprnds.fst, oprnds.snd);
  if (flags.modulo != 0)
    res = (long long int)modulo(res, flags.modulo);

  printf("%lli\n", res);

  return 0;
}
