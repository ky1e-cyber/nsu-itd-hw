#include <stddef.h>
#include <stddef.h>
#include <string.h>

#include "myblas.h"

static const size_t ELEM_SIZE = sizeof(double);

// скопировать вектор из X в Y
void dcopy(int n, const double* restrict X, double* restrict Y) {
  memcpy(Y, X, n * ELEM_SIZE);
}

// обменять местами содержимое векторов X и Y
void dswap(int n, double* restrict X, double* restrict Y) {
  for (size_t i = 0; i < n; i++) {
    double x = X[i];
    X[i] = Y[i];
    Y[i] = x;
  }
}

// домножить вектор X на коэффициент alpha
void dscal(int n, double alpha, double* X) {
  for (size_t i = 0; i < n; i++) {
    X[i] = alpha * X[i];
  }
}

// прибавить к вектору Y вектор X, умноженный на коэффициент alpha
void daxpy(int n, double alpha, const double* restrict X, double* restrict Y) {
  for (size_t i = 0; i < n; i++) {
    Y[i] += alpha * X[i];
  }
}

// вычислить скалярное произведение векторов X и Y
double ddot(int n, const double*  restrict X, const double* restrict Y) {
  double sum = 0;
  for (size_t i = 0; i < n; i++) {
    sum += X[i] * Y[i];
  }

  return sum;
}
