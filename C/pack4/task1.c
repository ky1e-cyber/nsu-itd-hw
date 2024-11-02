#include <stdio.h>
#include <stdlib.h>

typedef struct DateTime_s {
  int year, month, day;
  int hours, minutes, seconds;
} DateTime;

static inline unsigned int datetime_days(DateTime dt) {
  return 366 * (dt.year - 1) + 31 * (dt.month - 1) + dt.day;
}

static inline unsigned int datetime_seconds(DateTime dt) {
  return 60 * 60 * dt.hours + 60 * dt.minutes + dt.seconds;
}

// DateTime less than or eq (<=)
static inline int datetime_le(DateTime left, DateTime right) {
  unsigned int left_days = datetime_days(left);
  unsigned int right_days = datetime_days(right);

  return left_days == right_days
             ? datetime_seconds(left) <= datetime_seconds(right)
             : left_days < right_days;
}

DateTime min(const DateTime* arr, int cnt) {
  DateTime min = arr[0];

  for (int i = 1; i < cnt; i++) {
    if (datetime_le(arr[i], min)) {
      min = arr[i];
    }
  }

  return min;
}

int main() {
  size_t n;
  scanf("%lu", &n);

  DateTime* dates = (DateTime*)malloc(sizeof(DateTime) * n);

  for (size_t i = 0; i < n; i++) {
    scanf("%d %d %d %d %d %d", &dates[i].year, &dates[i].month, &dates[i].day,
          &dates[i].hours, &dates[i].minutes, &dates[i].seconds);
  }

  DateTime min_date = min(dates, n);

  printf("%d %d %d %d %d %d\n", min_date.year, min_date.month, min_date.day,
         min_date.hours, min_date.minutes, min_date.seconds);

  return 0;
}
