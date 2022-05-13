
top: reg

# regression tests...

reg: gen/zork.dis gen/zork.objects trace

gen/zork.dis: src/*.hs Makefile .gen
	stack run dis > $@

gen/zork.objects: src/*.hs Makefile .gen
	stack run objects > $@


trace: gen/zork.trace gen/zork.trace.invent

gen/zork.trace: story/zork1.88-840726.z3 run.sh src/*.hs Makefile .gen
	cat /dev/null | ./run.sh -trace $< > $@

gen/zork.trace.invent: story/zork1.88-840726.z3 run.sh src/*.hs Makefile .gen
	echo invent | ./run.sh -trace $< > $@

.gen:
	mkdir -p gen
