
top: test

test: run.out run.expected
	git diff --no-index run.expected run.out

run.out: src/*.hs Makefile
	(stack run > run.out ) || true
