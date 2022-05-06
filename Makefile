
top: test

test: run.out run.expected
	git diff --no-index run.expected run.out

run.expected: Makefile
	cat ~/projects/niz/story-dumps/zork1-88/zork.dis | tail +25 | head -38 > run.expected

run.out: src/*.hs Makefile
	(stack run > run.out ) || true
