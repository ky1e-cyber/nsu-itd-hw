#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

typedef const char* const conststr;

static conststr COMPILER =
#if defined(__clang__)
    "clang";
#elif defined(__TINYC__)
    "TCC";
#elif defined(__GNUC__)
    "GCC";
#endif

static size_t BITNESS = __WORDSIZE;

static conststr ASSERTS_ENABLED =
#if defined(NDEBUG)
    "disabled";
#else
    "enabled";
#endif

int main() {
  printf("Compiler: %s\nBitness: %llu\nAsserts: %s\n", COMPILER,
         (long long unsigned)BITNESS, ASSERTS_ENABLED);
  return 0;
}
