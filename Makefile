
top: dis-regression dev object-dump-regression

object-dump-regression: src/*.hs Makefile .gen
	stack run dump > gen/zork.objects
	git diff gen/zork.objects

dis-regression: src/*.hs Makefile .gen
	stack run dis-zork > gen/zork.dis
	git diff gen/zork.dis

.gen:
	mkdir -p gen

dev: run.out run.expected Makefile
	git diff --color --no-index run.expected run.out

run.expected: ~/niz.trace Makefile
	cat ~/niz.trace | tail +2 | head -234 > run.expected

run.out: src/*.hs Makefile
	stack build
	( stack run > run.out 2>&1  ) || true
