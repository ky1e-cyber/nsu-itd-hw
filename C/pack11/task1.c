#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

int compare_ints(const void* left, const void* right) {
  int leftint = *(const int*)left;
  int rightint = *(const int*)right;
  return (leftint > rightint) - (leftint < rightint);
}

static inline int correct_ind(int* xs, int size, int ind) {
  while (ind < size - 1 && xs[ind] == xs[ind + 1])
    ind++;
  return ind;
}

int main() {
  int n;
  scanf("%d", &n);
  int* as = (int*)malloc(sizeof(int) * n);
  for (int i = 0; i < n; i++)
    scanf("%d", as + i);

  int m;
  scanf("%d", &m);
  int* bs = (int*)malloc(sizeof(int) * m);
  for (int i = 0; i < m; i++)
    scanf("%d", bs + i);

  qsort(as, n, sizeof(int), compare_ints);
  qsort(bs, m, sizeof(int), compare_ints);

  int a_ind = correct_ind(as, n, 0);
  int b_ind = correct_ind(bs, m, 0);
  int res = 0;

  while (a_ind < n) {
    if (as[a_ind] < bs[b_ind]) {
      while (a_ind < n && (as[a_ind] < bs[b_ind])) {
        as[res++] = as[a_ind];
        a_ind = correct_ind(as, n, a_ind + 1);
      }
      if (a_ind >= n)
        break;
    } else if (as[a_ind] > bs[b_ind]) {
      while (b_ind < m && as[a_ind] > bs[b_ind]) {
        b_ind = correct_ind(bs, m, b_ind + 1);
      }
      if (b_ind >= m)
        break;
    } else {
      a_ind = correct_ind(as, n, a_ind + 1);
      b_ind = correct_ind(bs, m, b_ind + 1);
      if (b_ind >= m)
        break;
    }
  }

  while (a_ind < n) {
    as[res++] = as[a_ind];
    a_ind = correct_ind(as, n, a_ind + 1);
  }

  printf("%d\n", res);
  for (int i = 0; i < res; i++)
    printf("%d ", as[i]);
  puts("");

  free(as);
  free(bs);

  return 0;
}

