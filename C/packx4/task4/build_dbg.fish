clang -fsanitize=address -fsanitize=undefined core.c -shared -o core.so
clang -fsanitize=address -fsanitize=undefined main.c core.so -o main
