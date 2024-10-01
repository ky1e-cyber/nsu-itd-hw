#include <stdio.h>
#include <stdlib.h>

typedef struct {
  size_t l;
  size_t r;
  long long int sum;
} res_t;

res_t max_subarray(int* arr, size_t size) {
  long long int sum = arr[0];
  long long int best = arr[0];

  size_t best_l = 0;
  size_t best_r = 0;

  size_t current_l = 0;

  for (size_t i = 1; i < size; i++) {
    if (arr[i] > sum + arr[i]) {
      sum = arr[i];
      current_l = i;
    } else {
      sum = sum + arr[i];
    }

    if (sum > best) {
      best = sum;
      best_l = current_l;
      best_r = i;
    }
  }

  return (res_t){.l = best_l, .r = best_r, .sum = best};
}

int main() {
  size_t n;
  scanf("%lu", &n);

  int* arr = (int*)malloc(sizeof(int) * n);

  for (size_t i = 0; i < n; i++) {
    scanf("%i", arr + i);
  }

  res_t res = max_subarray(arr, n);

  printf("%lu %lu %lli\n", res.l, res.r, res.sum);

  return 0;
}
