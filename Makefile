
top: reg trace walk


reg: gen/zork.dis gen/zork.objects

gen/zork.dis: src/*.hs Makefile .gen
	stack run dis > $@

gen/zork.objects: src/*.hs Makefile .gen
	stack run objects > $@


trace: gen/zork.trace gen/zork.trace.invent gen/zork.trace.2

gen/zork.trace: story/zork1.88-840726.z3 src/*.hs Makefile .gen
	stack run -- -trace $< > $@

gen/zork.trace.invent: story/zork1.88-840726.z3 src/*.hs Makefile .gen
	stack run -- -type invent -trace $< > $@

gen/zork.trace.2: story/zork1.88-840726.z3 src/*.hs Makefile .gen
	stack run -- -walk inputs2 -trace $< > $@


walk: gen/zork.out1 gen/zork.out2

gen/zork.out1: story/zork1.88-840726.z3 src/*.hs Makefile .gen
	stack run -- -walk inputs1 $< > $@

gen/zork.out2: story/zork1.88-840726.z3 src/*.hs Makefile .gen
	stack run -- -walk inputs2 $< > $@


.gen:
	mkdir -p gen
