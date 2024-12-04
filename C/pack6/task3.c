#include <assert.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ARENA_LOG_PREFIX "[ARENA]"
#define ARENA_INIT_SIZE 2 << 20
#define DEBUG 0
#define LOG_FILE stdout

typedef uint8_t byte;

typedef struct __attribute__((packed, aligned(8))) {
  size_t size;  // size of arena
  size_t pos;   // (aligned) bytes allocated
} arena_header_t;

typedef arena_header_t* aptr_t;

static inline byte* arena_basepointer(aptr_t arena) {
  return (byte*)(arena + 1);
}

static inline size_t arena_isfits(aptr_t arena, size_t size) {
  size_t free_bytes = arena->size - arena->pos;
  return size <= free_bytes;
}

aptr_t arena_make(size_t size) {
#if DEBUG
  fprintf(LOG_FILE, ARENA_LOG_PREFIX " Creating arena\n");
#endif
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
#if DEBUG
    fprintf(LOG_FILE, ARENA_LOG_PREFIX " Unable to allocate in arena\n");
#endif
    return (void*)NULL;
  }

#if DEBUG
  fprintf(LOG_FILE, ARENA_LOG_PREFIX " Allocated in arena\n");
#endif

  void* mem = (void*)(arena_basepointer(arena) + (arena->pos));
  arena->pos = arena->pos + aligned_size;

  return mem;
}

aptr_t arena_grow(aptr_t arena, size_t new_size) {
#if DEBUG
  fprintf(LOG_FILE, ARENA_LOG_PREFIX " Growing arena\n");
#endif

  assert(new_size > arena->size);

  aptr_t newmem = (aptr_t)realloc(arena, sizeof(arena_header_t) + new_size);
  if (newmem == NULL)
    return NULL;
  newmem->size = new_size;

  return newmem;
}

void arena_free(aptr_t arena) {
#if DEBUG
  fprintf(LOG_FILE, ARENA_LOG_PREFIX " Freeing arena\n");
#endif

  free((void*)arena);
}

typedef struct {
  size_t size;
} string_header_t;

typedef string_header_t* strptr_t;

char* string_basepointer(strptr_t ptr) {
  return (char*)(ptr + 1);
}

strptr_t string_from_cstring_sized(strptr_t dest, char* source, size_t size) {
  memcpy(string_basepointer(dest), source, size * sizeof(char));
  dest->size = size;

  return dest;
}

strptr_t string_from_cstring(strptr_t dest, char* source) {
  size_t size = strlen(source);
  return string_from_cstring_sized(dest, source, size);
}

void string_fprint(FILE* file, strptr_t str) {
  size_t size = str->size;
  char* base = string_basepointer(str);
  for (size_t i = 0; i < size; i++) {
    fputc(base[i], file);
  }
}

uint64_t string_count(strptr_t str, char c) {
  size_t size = str->size;
  char* cstr = string_basepointer(str);
  uint64_t cnt = 0;
  for (size_t i = 0; i < size; i++) {
    if (cstr[i] == c)
      cnt++;
  }

  return cnt;
}

int64_t arena_insert(aptr_t arena, char* str, size_t size) {
  strptr_t newstr = (strptr_t)arena_alloc(
      arena, sizeof(string_header_t) + size * sizeof(char));
  if (newstr == NULL)
    return -1;

  (void)string_from_cstring_sized(newstr, str, size);

  return (int64_t)((byte*)newstr - arena_basepointer(arena));
}

ptrdiff_t table[100000];

int main() {
#if DEBUG
  FILE* input_file = stdin;
#else
  FILE* input_file = fopen("input.txt", "r");
#endif

#if DEBUG
  FILE* output_file = stdout;
#else
  FILE* output_file = fopen("output.txt", "w");
#endif

  aptr_t arena = arena_make(ARENA_INIT_SIZE);
  uint32_t ic = 0;

  uint32_t n;
  (void)fscanf(input_file, "%u", &n);

  for (uint32_t i = 0; i < n; i++) {
    int t;
    (void)fscanf(input_file, "%d", &t);

    if (t == 0) {
      char buf[100000 + 1];
      int l;
      (void)fscanf(input_file, "%d", &l);
      (void)fgetc(input_file);  // eat space
      (void)fgets(buf, l + 1, input_file);
      int64_t offset = arena_insert(arena, buf, l);
      if (offset == -1) {
        arena = arena_grow(arena, arena->size * 2);
        offset = arena_insert(arena, buf, l);
      }
      table[ic++] = offset;

    } else {
      int ind;
      (void)fscanf(input_file, "%d", &ind);

      if (t == 2) {
        string_fprint(output_file,
                      (strptr_t)(arena_basepointer(arena) + table[ind]));
        (void)fputs("\n", output_file);
      } else if (t == 3) {
        (void)fgetc(input_file);  // eat space
        char c = fgetc(input_file);
        (void)fprintf(
            output_file, "%llu\n",
            (unsigned long long)string_count(
                (strptr_t)(arena_basepointer(arena) + table[ind]), c));
      }
    }
  }

  arena_free(arena);

#if DEBUG
#else
  (void)fclose(input_file);
  (void)fclose(output_file);
#endif

  return 0;
}
