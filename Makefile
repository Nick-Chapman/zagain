
top: reg

# regression
reg: reg-objects reg-dis reg-trace

reg-objects: src/*.hs Makefile .gen
	stack run objects > gen/zork.objects
	git diff gen/zork.objects

reg-dis: src/*.hs Makefile .gen
	stack run dis > gen/zork.dis
	git diff gen/zork.dis

reg-trace: src/*.hs Makefile .gen
	stack run trace > gen/zork.trace
	git diff gen/zork.trace

reg-trace1: src/*.hs Makefile .gen
	stack run > gen/zork.trace1
	git diff gen/zork.trace1

.gen:
	mkdir -p gen

# ongoing dev...
dev: run.out run.expected Makefile
	git diff --color --no-index run.expected run.out

run.expected: run.expected.full Makefile
	cat run.expected.full | tail +2 | head -410 > run.expected

run.out: src/*.hs Makefile
	stack build
	( stack run > run.out 2>&1  ) || true
