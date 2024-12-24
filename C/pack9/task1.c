#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// merges sorted arrays a[0..ak-1] and b[0..bk-1] into
// one sorted array res[0..rk-1], returning rk from function
int merge(const int* a, int ak, const int* b, int bk, int* res) {
  int a_ind = 0;
  int b_ind = 0;
  int res_ind = 0;

  while (a_ind < ak && b_ind < bk) {
    int nxt = a[a_ind] < b[b_ind] ? a[a_ind++] : b[b_ind++];
    res[res_ind++] = nxt;
  }

  while (a_ind < ak)
    res[res_ind++] = a[a_ind++];

  while (b_ind < bk)
    res[res_ind++] = b[b_ind++];

  return res_ind;
}

int main() {
  FILE* in = fopen("input.txt", "rb");
  FILE* out = fopen("output.txt", "wb");

  uint32_t n, m;
  fread(&n, 4, 1, in);
  fread(&m, 4, 1, in);

  int32_t* as = (int32_t*)malloc(n * 4);
  int32_t* bs = (int32_t*)malloc(m * 4);
  int32_t* res = (int32_t*)malloc((n + m) * 4);

  fread(as, 4, n, in);
  fread(bs, 4, m, in);

  int rk = merge(as, n, bs, m, res);

  fwrite(res, 4, rk, out);

  free(as);
  free(bs);
  free(res);
  fclose(in);
  fclose(out);

  return 0;
}
