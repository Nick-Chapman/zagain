
top: run-hello

GCC_FLAGS = -Wall -Werror

run-%: %.exe
	./$^

%.exe : %.c
	gcc $(GCC_FLAGS) $< -o $@
