CFLAGS_LOCAL=-Wall -Werror -fsanitize=undefined -fsanitize=address

all: local myblas

local:
	gcc $(CFLAGS_LOCAL) main.c matrixsqr.c -L. -lopenblas -g -lpthread -o checked_solution1

myblas: myblas.c
	mkdir -p ./myblas
	gcc myblas.c -shared -fPIC -o ./myblas/libopenblas.so

