
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

.gen:
	mkdir -p gen
