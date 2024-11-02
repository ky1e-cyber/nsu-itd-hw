#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

static inline size_t pascaltrig_get_index(size_t n, size_t k) {
  return ((n + 1) * n) / 2 + k;
}

void pascaltrig_fill(double* trig_buf, size_t size) {
  assert(size >= 3);

  trig_buf[0] = 1;
  trig_buf[1] = 1;
  trig_buf[2] = 1;

  // leftmost

  size_t n_leftmost = 2;
  for (size_t leftmost_ind = pascaltrig_get_index(n_leftmost, 0);
       leftmost_ind < size;
       leftmost_ind = pascaltrig_get_index(++n_leftmost, 0)) {

    trig_buf[leftmost_ind] = 1;
  }

  // diag
  size_t n_diag = 1;
  for (size_t diag_ind = 5; diag_ind < size;
       diag_ind = pascaltrig_get_index(n_diag, n_diag)) {
    trig_buf[diag_ind] = 1;
    n_diag++;
  }

  size_t n = 2;
  size_t k = 1;
  size_t ind = pascaltrig_get_index(n, k);
  while (ind < size) {
    if (k <= (n - 1)) {
      trig_buf[ind] =
          (k == 0 ? 0 : trig_buf[pascaltrig_get_index(n - 1, k - 1)]) +
          trig_buf[pascaltrig_get_index(n - 1, k)];
      k++;
    } else {
      k = 1;
      n += 1;
    }
    ind = pascaltrig_get_index(n, k);
  }
}

int main() {
  size_t n;
  scanf("%lu", &n);

  size_t* indecies = (size_t*)malloc(sizeof(size_t) * n);

  size_t max_ind = 2;

  for (size_t i = 0; i < n; i++) {
    size_t n, k;
    scanf("%lu %lu", &n, &k);
    size_t ind = pascaltrig_get_index(n, k);
    if (ind > max_ind)
      max_ind = ind;
    indecies[i] = ind;
  }

  double* trig =
      (double*)malloc(sizeof(double) * (max_ind + 1));

  pascaltrig_fill(trig, max_ind + 1);

  for (size_t i = 0; i < n; i++) {
    double coef = trig[indecies[i]];
    printf("%0.10g\n", coef);
  }

  return 0;
}
