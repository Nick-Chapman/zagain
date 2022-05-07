
top: regression

regression: src/*.hs Makefile .gen
	stack run dis-zork > gen/zork.dis
	git diff gen/zork.dis

.gen:
	mkdir -p gen
