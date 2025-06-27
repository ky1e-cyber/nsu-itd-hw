#include <stdio.h>

static const int MODULO = 1000000007;

static int partition_count_leq_k(int n, int k) {
  if (k > n)
    return partition_count_leq_k(n, n);

  if (k > 0) {
    int no_k_cnt = partition_count_leq_k(n, k - 1);
    int with_k_cnt = partition_count_leq_k(n - k, k);
    return (no_k_cnt + with_k_cnt) % MODULO;
  }

  return n == 0 ? 1 : 0;
}

int main() {
  int n;
  scanf("%d", &n);

  printf("%d", partition_count_leq_k(n, n));

  return 0;
}
