#include <stddef.h>
#include "myblas.h"

// вычислить матрицу (alpha*A*B + beta*C) и записать её в C
// здесь A –- матрица размера m на k, B –- матрица размера k на n,
// C –- матрица размера m на n
void dgemm(int m,
           int n,
           int k,
           double alpha,
           const double* restrict A,
           const double* restrict B,
           double beta,
           double* C) {
  for (size_t i = 0; i < m; i++) {
    for (size_t j = 0; j < n; j++) {
      double dot = 0;
      for (size_t t = 0; t < k; t++) {
        dot += A[i * k + t] * B[t * n + j];
      }
      dot *= alpha;
      C[i * n + j] = beta * C[i * n + j] + dot;
    }
  }
}
