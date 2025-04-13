#include <stddef.h>
#include "myblas.h"

// вычислить вектор (alpha*A*X + beta*Y) длины m, и записать его в Y
// здесь A –- матрица размера m на n, X –- вектор длины n, а Y –- вектор длины m
void dgemv(int m,
           int n,
           double alpha,
           const double* restrict A,
           const double* restrict X,
           double beta,
           double* Y) {
  for (size_t i = 0; i < m; i++) {
    double t = beta * Y[i];
    double r = alpha * ddot(n, A + i * n, X);
    Y[i] = t + r;
  }
}

// вычислить матрицу (alpha*X*Yt + A) и записать её в A
// здесь Yt –- это транспонированный вектор Y, то есть записанный как
// вектор-строка
//  A –- матрица размера m на n, X –- вектор длины m, а Y –- вектор длины n
void dger(int m,
          int n,
          double alpha,
          const double* restrict X,
          const double* restrict Y,
          double* A) {
  for (size_t i = 0; i < m; i++) {
    for (size_t j = 0; j < n; j++) {
      double r = alpha * X[i] * Y[j];
      A[i * n + j] += r;
    }
  }
}
