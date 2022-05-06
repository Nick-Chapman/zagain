
top: test

test: run.out run.expected
	git diff --no-index run.expected run.out

run.expected: Makefile
	cat ~/projects/niz/story-dumps/zork1-88/zork.dis | tail +25 | head -32 > run.expected

run.out: src/*.hs Makefile
	(stack run dis1 > run.out ) || true
