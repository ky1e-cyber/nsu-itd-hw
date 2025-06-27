#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "core.h"
#include "state.h"

static inline void echo(const char* args[], const size_t args_sz) {
  printf("ECHO: ");
  if (args_sz > 0) {
    for (size_t i = 0; i < args_sz - 1; i++) {
      printf("%s|", args[i]);
    }
    printf("%s", args[args_sz - 1]);
  }

  printf("\n");
}

static uint8_t parse_id(const char* idx) {
  int i = atoi(idx);
  return (uint8_t)i;
}

static void store_reg(State* state, uint8_t dst_id, char* what) {
  char* cont = state->regs[dst_id];

  if (cont != NULL)
    free(cont);

  size_t len = strlen(what);

  state->regs[dst_id] = (char*)malloc(sizeof(char) * (len + 1));
  if (state->regs[dst_id] == NULL) {
    fprintf(stderr, "Failed to allocate memory for register %u\n",
            (unsigned int)dst_id);
    exit(1);
  }

  memcpy(state->regs[dst_id], what, sizeof(char) * (len + 1));
}

// prints ’ECHO: ’ and all passed strings separated by ’|’
void echo_0(State* state) {
  echo(NULL, 0);
}

void echo_1(State* state, char* arg0) {
  const char* args[] = {arg0};
  echo(args, 1);
}

void echo_2(State* state, char* arg0, char* arg1) {
  const char* args[] = {arg0, arg1};
  echo(args, 2);
}

void echo_3(State* state, char* arg0, char* arg1, char* arg2) {
  const char* args[] = {arg0, arg1, arg2};
  echo(args, 3);
}

// prints contents of I-th register (it must not be NULL)
//[idx] contains decimal representation of I
void print_1(State* state, char* idx) {
  uint8_t id = parse_id(idx);
  const char* s = state->regs[id];
  if (s == NULL) {
    fprintf(stderr, "Trying address register %u which is unset\n",
            (unsigned int)id);
    exit(1);
  }

  printf("%s\n", s);
}

// prints all non-NULL registers with their values (sorted by register number)
void printregs_0(State* state) {
  for (int i = 0; i < 256; i++) {
    if (state->regs[i] != NULL)
      printf("%d = %s\n", i, state->regs[i]);
  }
}

// saves a copy of string [what] into I-th register
//[idx] contains decimal representation of I
void store_2(State* state, char* idx, char* what) {
  uint8_t id = parse_id(idx);
  store_reg(state, id, what);
}

// copies contents of S-th register into D-th register (S-th register is not
// NULL) [dst] and [src] contain decimal representations of D and S respectively
// BEWARE: [dst] and [src] are allowed to be equal indices
void copy_2(State* state, char* dst, char* src) {
  uint8_t dst_id = parse_id(dst);
  uint8_t src_id = parse_id(src);

  if (src_id == dst_id)
    return;

  store_reg(state, dst_id, state->regs[src_id]);
}

// assigns NULL to I-th register
//[idx] contains decimal representation of I
void clear_1(State* state, char* idx) {
  uint8_t id = parse_id(idx);
  free(state->regs[id]);
  state->regs[id] = NULL;
}
