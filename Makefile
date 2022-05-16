
top: dev


dev: my.walk mojo.walk
	git diff --no-index my.walk mojo.walk

my.walk: z.script Makefile src/*.hs
	(stack run -- -walk z.script -nobuf > my.walk 2>&1) || true

mojo.walk:
	(cd ~/code/other/mojozork; make && ./mojozork.exe ~/code/zagain/story/zork1.88-840726.z3 zork1-script.txt) > mojo.walk


reg: gen/zork.dis gen/zork.objects gen/zork.trace gen/zork.trace.invent gen/zork.trace.2 gen/zork.out1 gen/zork.out2
	git diff gen

gen/zork.dis: src/*.hs Makefile .gen
	stack run dis > $@

gen/zork.objects: src/*.hs Makefile .gen
	stack run objects > $@

gen/zork.trace: story/zork1.88-840726.z3 src/*.hs Makefile .gen
	stack run -- -trace $< > $@

gen/zork.trace.invent: story/zork1.88-840726.z3 src/*.hs Makefile .gen
	stack run -- -type invent -trace $< > $@

gen/zork.trace.2: story/zork1.88-840726.z3 src/*.hs Makefile .gen
	stack run -- -walk inputs2 -trace $< > $@


gen/zork.out1: story/zork1.88-840726.z3 src/*.hs Makefile .gen
	stack run -- -walk inputs1 $< > $@

gen/zork.out2: story/zork1.88-840726.z3 src/*.hs Makefile .gen
	stack run -- -walk inputs2 $< > $@


.gen:
	mkdir -p gen
