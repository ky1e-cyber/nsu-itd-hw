#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

typedef enum { ALLOWED = 0, FORBIDDEN, QUEEN } cell_t;

cell_t g_final_field[12][12];

static void copy_field(cell_t* dst, const cell_t* src, int m, int n) {
  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      dst[i * 12 + j] = src[i * 12 + j];
    }
  }
}

static void fill_queen(cell_t* field,
                       int m,
                       int n,
                       int queen_row,
                       int queen_column) {
  assert(queen_row < m);
  assert(queen_column < n);
  assert(field[queen_row * 12 + queen_column] == QUEEN);

  // field[queen_row * 12 + queen_column] = FORBIDDEN;

  int curr_row = queen_row + 1;
  int curr_delta = 1;

  while (curr_row < m) {
    field[curr_row * 12 + queen_column] = FORBIDDEN;
    if ((queen_column + curr_delta) < n)
      field[curr_row * 12 + (queen_column + curr_delta)] = FORBIDDEN;
    if ((queen_column - curr_delta) >= 0)
      field[curr_row * 12 + (queen_column - curr_delta)] = FORBIDDEN;

    curr_row++;
    curr_delta++;
  }
}

static bool fillable(const cell_t* curr_field, int m, int n, int curr_row) {
  if (curr_row >= m) {
    copy_field(&g_final_field[0][0], curr_field, m, n);
    return true;
  }
  for (int i = 0; i < n; i++) {
    if (curr_field[curr_row * 12 + i] == FORBIDDEN)
      continue;
    cell_t new_field[12][12];
    copy_field(&new_field[0][0], curr_field, m, n);

    new_field[curr_row][i] = QUEEN;

    fill_queen(&new_field[0][0], m, n, curr_row, i);

    if (fillable(&new_field[0][0], m, n, curr_row + 1))
      return true;
  }

  return false;
}

static void read_field(cell_t* dst, int m, int n) {
  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      int c = fgetc(stdin);
      if (c == '?') {
        dst[i * 12 + j] = ALLOWED;
      } else {
        dst[i * 12 + j] = FORBIDDEN;
      }
    }
    int c = fgetc(stdin);
    assert(c == '\n');
  }
}

int main() {
  int m, n;
  scanf("%d %d\n", &m, &n);

  cell_t init_field[12][12];
  read_field(&init_field[0][0], m, n);

  if (fillable(&init_field[0][0], m, n, 0)) {
    puts("YES");

    for (int i = 0; i < m; i++) {
      for (int j = 0; j < n; j++) {
        if (g_final_field[i][j] == QUEEN) {
          fputc('X', stdout);
        } else {
          fputc('.', stdout);
        }
      }
      fputc('\n', stdout);
    }
  } else {
    puts("NO");
  }

  return 0;
}
