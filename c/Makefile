
top: _build/hello.js _build/goodbye.js _build/cube.js _build/mine.js

GCC_FLAGS = -Wall -Werror

.SECONDARY:

run-native-%: _build/%.exe
	$^

run-node-%: _build/%.js
	node $^

_build/%.exe : %.c .dir
	gcc $(GCC_FLAGS) $< -o $@

_build/%.js : %.c .dir
	emcc $< -o $@

_build/%.js : %.cpp .dir
	emcc $< -o $@

.dir:
	mkdir -p _build
