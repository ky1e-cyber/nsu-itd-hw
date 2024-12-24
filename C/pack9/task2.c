#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void swap_ints(int* a, int* b) {
  if (a == b)
    return;

  int t = *a;
  *a = *b;
  *b = t;
}

// partitions array a[0..n-1] into two subarrays, returning value k
// the subarray a[0..k-1] must have all elements <= pivot
// the subarray a[k..n-1] must have all elements >= pivot
int partition(int* a, int n, int pivot) {
  int balance = 0;

  int k = 0;

  for (int i = 0; i < n; i++) {
    if (a[i] < pivot) {
      swap_ints(a + k, a + i);
      k++;
    } else if (a[i] == pivot) {
      if (balance >= 1) {
        swap_ints(a + k, a + i);
        k++;
        balance--;
      } else {
        balance++;
      }
    }
  }

  return k;
}

int main() {
  FILE* in = fopen("input.txt", "rb");
  FILE* out = fopen("output.txt", "wb");

  uint32_t n;
  int32_t p;
  fread(&n, 4, 1, in);
  fread(&p, 4, 1, in);

  int32_t* xs = (int32_t*)malloc(n * 4);
  fread(xs, 4, n, in);

  int32_t k = (int32_t)partition(xs, n, p);

  fwrite(&k, 4, 1, out);
  fwrite(xs, 4, n, out);

  free(xs);
  fclose(in);
  fclose(out);

  return 0;
}
