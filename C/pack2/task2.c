#include <stdio.h>
#include <stdlib.h>

int main() {
  size_t n;
  scanf("%lu", &n);

  int* arr = (int*)malloc(sizeof(int) * n);

  for (size_t i = 0; i < n; i++) {
    scanf("%i", arr + i);
  }

  for (size_t k = 1; k <= n; k++) {
    long long int sum = 0;
    for (size_t i = k - 1; i < n; i = i + k) {
      sum += arr[i];
    }
    printf("%lld\n", sum);
  }

  return 0;
}
