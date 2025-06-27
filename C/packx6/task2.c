#include <stdio.h>
#include <stdlib.h>

#define m_min(x, y)             \
  ({                            \
    const __auto_type x_ = (x); \
    const __auto_type y_ = (y); \
    x_ < y_ ? x_ : y_;          \
  })

static const int MODULO = 1000000007;

static int partition_count_leq_k(int sum, int k, int* cache, int n) {
  if (cache[sum * n + k] == -1) {
    int res = 0;
    if (sum == n) {
      res = 1;
    } else {
      for (int i = 1; i <= m_min(k, n - sum); i++) {
        res = (res + partition_count_leq_k(sum + i, i, cache, n)) % MODULO;
      }
    }

    cache[sum * n + k] = res;
  }

  return cache[sum * n + k];
}

int main() {
  int n;
  scanf("%d", &n);

  int* cache = (int*)malloc(sizeof(int) * (n + 1) * (n + 1));
  if (cache == NULL)
    exit(1);

  for (int i = 0; i < ((n + 1) * (n + 1)); i++)
    cache[i] = -1;

  printf("%d", partition_count_leq_k(0, n, cache, n));

  return 0;
}
