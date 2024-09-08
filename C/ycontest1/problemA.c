/*
В офисе, где работает программист Петр, установили кондиционер нового типа.
Этот кондиционер отличается особой простотой в управлении.
У кондиционера есть всего лишь два управляемых параметра: желаемая температура и
режим работы.

Кондиционер может работать в следующих четырех режимах:
«freeze» — охлаждение. В этом режиме кондиционер может только уменьшать
температуру. Если температура в комнате и так не больше желаемой, то он
выключается.

«heat» — нагрев. В этом режиме кондиционер может только увеличивать температуру.
Если температура в комнате и так не меньше желаемой, то он выключается.

«auto» — автоматический режим. В этом режиме кондиционер может как увеличивать,
так и уменьшать температуру в комнате до желаемой.

«fan» — вентиляция. В этом режиме кондиционер осуществляет
только вентиляцию воздуха и не изменяет температуру в комнате.

Кондиционер достаточно мощный, поэтому при настройке на правильный режим работы
он за час доводит температуру в комнате до желаемой.
Требуется написать программу, которая по заданной температуре в комнате troom,
установленным на кондиционере желаемой температуре tcond и режиму работы
определяет температуру, которая установится в комнате через час.

Формат ввода:
Первая строка входного файла содержит два целых числа troom, и tcond,
разделенных ровно одним пробелом
(-50 <= t_toom <= 50; -50 <= t_cond <= 50).

Вторая строка содержит одно слово,
записанное строчными буквами латинского алфавита - режим работы кондиционера.

Формат вывода:
Выходной файл должен содержать одно целое число - температуру,
которая установится в комнате через час.
*/

#include <stdio.h>

typedef signed char s8;

// Using inlines to get rid of
// unnecessary calls and
// get remove recalculations

static inline int max(int a, int b) {
  return a > b ? a : b;
}

static inline int min(int a, int b) {
  return a > b ? b : a;
}

int main() {
  // I discovered that GCC actually allocate
  // the same amount of memory as for int
  // so limiting size to s8 is absolutely meaningless :)
  s8 t_room, t_cond;
  (void)scanf("%d %d", (int*)&t_room, (int*)&t_cond);

  char mode_buf[2];
  (void)getc(stdin);

  mode_buf[0] = getc(stdin);
  mode_buf[1] = getc(stdin);

  /*
    We actually need only 2 chars from stream
    since we can deduce what
    mode specified from only 2 symbols:
    [fr]eeze
    [fa]n
    [he]at
    [au]to
  */

  s8 t_res = t_room;

  // filter the f[a]n mode
  if (mode_buf[1] != 'a') {
    switch (mode_buf[0]) {
      case 'a':
        if (t_room > t_cond) {
          goto freeze_label;
        }
      case 'h':
        t_res = max(t_room, t_cond);
        break;
      case 'f':
      freeze_label:
        t_res = min(t_room, t_cond);
        break;
    }
  }

  printf("%d\n", t_res);
}
