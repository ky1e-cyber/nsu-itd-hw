#include <stdio.h>
#include <stdlib.h>

typedef enum CBLAS_ORDER {
  CblasRowMajor = 101,
  CblasColMajor = 102
} CBLAS_ORDER;

typedef CBLAS_ORDER CBLAS_LAYOUT;

typedef enum CBLAS_TRANSPOSE {
  CblasNoTrans = 111,
  CblasTrans = 112,
  CblasConjTrans = 113,
  CblasConjNoTrans = 114
} CBLAS_TRANSPOSE;

typedef int MKL_INT;

static void not_supported() {
  fprintf(stderr, "Not supported\n ");
  exit(1);
}

void cblas_dgemm(const CBLAS_LAYOUT Layout,
                 const CBLAS_TRANSPOSE transa,
                 const CBLAS_TRANSPOSE transb,
                 const MKL_INT m,
                 const MKL_INT n,
                 const MKL_INT k,
                 const double alpha,
                 const double* a,
                 const MKL_INT lda,
                 const double* b,
                 const MKL_INT ldb,
                 const double beta,
                 double* c,
                 const MKL_INT ldc) {
  if (Layout != CblasRowMajor && transa != CblasNoTrans &&
      transb != CblasNoTrans)
    not_supported();

  if (m != n || n != k || k != m)
    not_supported();

  if (lda != ldb)
    not_supported();

  if (m != lda)
    not_supported();

  if (alpha != 1.0)
    not_supported();

  if (beta != 0.0)
    not_supported();

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      double prod = 0;
      for (int k = 0; k < n; k++) {
        prod += a[i * n + k] * b[k * n + j];
      }
      c[i * n + j] = prod;
    }
  }
}
