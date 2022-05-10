
top: dis-regression dev

dis-regression: src/*.hs Makefile .gen
	stack run dis-zork > gen/zork.dis
	git diff gen/zork.dis

.gen:
	mkdir -p gen

dev: run.out run.expected Makefile
	git diff --color --no-index run.expected run.out

run.expected: ~/niz.trace Makefile
	#cat ~/niz.trace | head -400 | sed 's/Decode [^ ]*/Decode/' > run.expected
	cat ~/niz.trace | tail +2 | head -222 > run.expected

run.out: src/*.hs Makefile
	stack build
	( stack run > run.out 2>&1  ) || true
