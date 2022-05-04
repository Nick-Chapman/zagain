
top: run-node-hello

GCC_FLAGS = -Wall -Werror

run-native-%: %.exe
	./$^

%.exe : %.c
	gcc $(GCC_FLAGS) $< -o $@


run-node-%: %.js
	node ./$^

hello.js : hello.c
	emcc hello.c -o hello.html
