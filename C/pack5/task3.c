#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

static uint8_t hexchar_to_u8(char c) {
  if (c >= '0' && c <= '9')
    return c - '0';
  if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  return 0;
}

typedef struct {
  int* mem;
  size_t size;
  size_t fron_ind;
  size_t back_ind;
  bool is_empty;
} ringbuffer_t;

void ringbuffer_init(ringbuffer_t* rb, size_t size, size_t init_ind) {
  assert(init_ind < size);
  int* m = (int*)malloc(sizeof(int) * size);
  assert(m != NULL);  // not ok but will do

  rb->mem = m;
  rb->size = size;
  rb->fron_ind = rb->back_ind = init_ind;
  rb->is_empty = true;
}

void ringbuffer_pushback(ringbuffer_t* rb, int elem) {
  size_t ind = (rb->back_ind + 1) % rb->size;
  if (rb->is_empty) {
    ind = rb->back_ind;
    rb->is_empty = false;
  } else if (ind == rb->fron_ind) {
    rb->fron_ind = (rb->fron_ind + 1) % rb->size;
  }

  rb->mem[ind] = elem;
  rb->back_ind = ind;
}

void ringbuffer_pushfront(ringbuffer_t* rb, int elem) {
  size_t ind = (rb->fron_ind == 0) ? rb->size - 1 : rb->fron_ind - 1;
  if (rb->is_empty) {
    ind = rb->fron_ind;
    rb->is_empty = false;
  } else if (ind == rb->back_ind) {
    rb->back_ind = (rb->back_ind == 0) ? rb->size - 1 : rb->back_ind - 1;
  }

  rb->mem[ind] = elem;
  rb->fron_ind = ind;
}

int ringbuffer_popback(ringbuffer_t* rb) {
  assert(!(rb->is_empty));

  int r = rb->mem[rb->back_ind];

  if (rb->back_ind == rb->fron_ind) {
    rb->is_empty = true;
  } else {
    rb->back_ind = (rb->back_ind == 0) ? rb->size - 1 : rb->back_ind - 1;
  }

  return r;
}

int ringbuffer_popfront(ringbuffer_t* rb) {
  assert(!(rb->is_empty));

  int r = rb->mem[rb->fron_ind];

  if (rb->fron_ind == rb->back_ind) {
    rb->is_empty = true;
  } else {
    rb->fron_ind = (rb->fron_ind + 1) % rb->size;
  }

  return r;
}

size_t ringbuffer_len(ringbuffer_t* rb) {
  if (rb->is_empty)
    return 0;
  else if (rb->fron_ind == rb->back_ind)
    return 1;

  size_t cnt = 0;
  for (size_t i = rb->fron_ind; i != rb->back_ind; i = (i + 1) % rb->size)
    cnt++;

  return cnt + 1;
}

void ringbuffer_print(ringbuffer_t* rb) {
  if (rb->is_empty) {
    puts("");
    return;
  } else if (rb->fron_ind == rb->back_ind) {
    printf("%d\n", rb->mem[rb->fron_ind]);
    return;
  }

  for (size_t i = rb->fron_ind; i != rb->back_ind; i = (i + 1) % rb->size) {
    printf("%d ", rb->mem[i]);
  }

  printf("%d\n", rb->mem[rb->back_ind]);
}

typedef enum {
  POPBACK,
  POPFRONT,
} pop_cmd_t;

typedef enum {
  PUSHBACK,
  PUSHFRONT,
} push_cmd_t;

typedef struct {
  pop_cmd_t pop_cmd;
  push_cmd_t push_cmd;
  ringbuffer_t* rb_from;
  ringbuffer_t* rb_to;
} op_t;

pop_cmd_t parse_pop_cmd(int n) {
  return (n & 0b0100) ? POPBACK : POPFRONT;
}

push_cmd_t parse_push_cmd(int n) {
  return (n & 0b0001) ? PUSHBACK : PUSHFRONT;
}

static inline ringbuffer_t* choose_buf_from(ringbuffer_t* left,
                                          ringbuffer_t* right,
                                          uint8_t n) {
  return (n & 0b1000) ? right : left;
}

static inline ringbuffer_t* choose_buf_to(ringbuffer_t* left,
                                            ringbuffer_t* right,
                                            int n) {
  return (n & 0b0010) ? right : left;
}

static inline void op_exec(op_t op) {
  if (op.rb_from->is_empty)
    return;

  int card;

  switch (op.pop_cmd) {
    case POPBACK:
      card = ringbuffer_popback(op.rb_from);
      break;
    case POPFRONT:
      card = ringbuffer_popfront(op.rb_from);
      break;
    default:
      return;
  }

  switch (op.push_cmd) {
    case PUSHBACK:
      ringbuffer_pushback(op.rb_to, card);
      break;
    case PUSHFRONT:
      ringbuffer_pushfront(op.rb_to, card);
      break;
    default:
      return;
  }
}

int main() {
  size_t n, m;
  scanf("%lu %lu", &n, &m);

  ringbuffer_t rb_l;
  ringbuffer_init(&rb_l, 2 * n + 1, n - 1);

  ringbuffer_t rb_r;
  ringbuffer_init(&rb_r, 2 * n + 1, n - 1);

  for (int i = 1; i <= (int)n; i++) {
    ringbuffer_pushback(&rb_l, i);
  }

  for (int i = -1; i >= -((int)n); i--) {
    ringbuffer_pushback(&rb_r, i);
  }

  (void)fgetc(stdin);

  for (size_t i = 0; i < m; i++) {
    char c = fgetc(stdin);
    uint8_t n = hexchar_to_u8(c);
    op_t op = (op_t){.pop_cmd = parse_pop_cmd(n),
                     .push_cmd = parse_push_cmd(n),
                     .rb_from = choose_buf_from(&rb_l, &rb_r, n),
                     .rb_to = choose_buf_to(&rb_l, &rb_r, n)};

    op_exec(op);
  }

  printf("%lu ", ringbuffer_len(&rb_l));
  ringbuffer_print(&rb_l);

  printf("%lu ", ringbuffer_len(&rb_r));
  ringbuffer_print(&rb_r);

  return 0;
}
