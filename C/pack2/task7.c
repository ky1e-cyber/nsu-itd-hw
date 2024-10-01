#include <stdio.h>
#include <stdlib.h>

int* merge_sort(int* nums, size_t nums_size, int* dest, size_t dest_size);
int* merge_sort_inplace(int* nums,
                        size_t nums_size,
                        int* buff,
                        size_t buff_size);

void swap(int* a, int* b) {
  int c = *a;
  *a = *b;
  *b = c;
}

int* bubble_propagation(int* nums, size_t nums_size, size_t elem_index) {
  while ((elem_index < (nums_size - 1)) &&
         (nums[elem_index] > nums[elem_index + 1])) {
    swap(nums + (elem_index), nums + (elem_index + 1));
    elem_index++;
  }

  return nums;
}

void merge(int* xs, size_t xs_size, int* ys, size_t ys_size, int* dest) {
  size_t xs_ind = 0;
  size_t ys_ind = 0;
  size_t ind = 0;

  while (xs_ind < xs_size && ys_ind < ys_size)
    swap(dest + (ind++),
         xs[xs_ind] < ys[ys_ind] ? (xs + (xs_ind++)) : (ys + (ys_ind++)));

  while (xs_ind < xs_size) {
    swap(dest + (ind++), xs + (xs_ind++));
  }

  while (ys_ind < ys_size)
    swap(dest + (ind++), ys + (ys_ind++));
}

int* merge_sort(int* nums, size_t nums_size, int* dest, size_t dest_size) {
  if (nums_size <= 1) {
    swap(nums, dest);
    return dest;
  }

  if (nums_size == 2) {
    size_t ind_of_less = nums[0] < nums[1] ? 0 : 1;
    swap(nums + ind_of_less, dest);
    swap(nums + (1 - ind_of_less), dest + 1);
    return dest;
  }

  size_t pivot = nums_size / 2;
  // size_t ys_size = nums_size - pivot;
  // size_t xs_sizes = nums_size - ys_size;

  merge_sort_inplace(nums, pivot, dest, dest_size);
  merge_sort_inplace(nums + pivot, nums_size - pivot, dest, dest_size);

  merge(nums, pivot, nums + pivot, nums_size - pivot, dest);
  return dest;
}

int* merge_sort_inplace(int* nums,
                        size_t nums_size,
                        int* buff,
                        size_t buff_size) {
  if (nums_size <= 1)
    return nums;

  if ((nums_size == 2) && (nums[0] > nums[1]))
    swap(nums, nums + 1);

  merge_sort(nums, nums_size, buff, buff_size);

  for (size_t i = 0; i < nums_size; i++)
    swap(buff + i, nums + i);

  return nums;
}

void sort_array(int* nums, size_t nums_size) {
  if (nums_size == 1)
    return;

  if (nums_size == 2) {
    if (nums[0] > nums[1]) {
      swap(nums, nums + 1);
    }
    return;
  }

  size_t buff_start = (nums_size / 2) + (nums_size % 2);
  merge_sort(nums, nums_size - buff_start, nums + buff_start,
             nums_size - buff_start);

  size_t size_left_unsorted = buff_start;

  while (size_left_unsorted > 1) {
    size_t pivot =
        (size_left_unsorted / 2) + (size_left_unsorted % 2);  // right start
    size_t buff_size = pivot;
    size_t arr_size = size_left_unsorted - buff_size;

    merge_sort(nums + pivot, arr_size, nums, buff_size);
    merge(nums, arr_size, nums + size_left_unsorted,
          nums_size - size_left_unsorted, nums + buff_size);

    size_left_unsorted = buff_size;
  }

  bubble_propagation(nums, nums_size, 0);
}

int main() {
  size_t n;
  scanf("%lu", &n);

  int* arr = (int*)malloc(sizeof(int) * n);

  for (size_t i = 0; i < n; i++) {
    scanf("%d", arr + i);
  }

  sort_array(arr, n);

  for (size_t i = 0; i < n - 1; i++) {
    printf("%d ", arr[i]);
  }
  printf("%d\n", arr[n - 1]);

  return 0;
}
