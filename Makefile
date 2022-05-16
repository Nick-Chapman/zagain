
top: dev

exe = .stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/main.exe/main.exe

dev: my.walk mojo.walk Makefile
	git diff --no-index my.walk mojo.walk

my.walk: $(exe) z.script Makefile
	($(exe) -walk z.script -nobuf > my.walk 2>&1) || true

mojo.walk: .mojo z.script Makefile
	~/code/other/mojozork/mojozork.exe ./story/zork1.88-840726.z3 z.script > mojo.walk

.mojo: Makefile
	(cd ~/code/other/mojozork; make)



reg: gen/zork.dis gen/zork.objects gen/zork.trace gen/zork.trace.invent gen/zork.trace.2 gen/zork.out1 gen/zork.out2
	git diff gen

gen/zork.dis: $(exe) src/*.hs Makefile .gen
	$(exe) dis > $@

gen/zork.objects: $(exe) src/*.hs Makefile .gen
	$(exe) objects > $@

gen/zork.trace:  src/*.hs $(exe) Makefile .gen
	$(exe) -trace > $@

gen/zork.trace.invent: $(exe) src/*.hs Makefile .gen
	$(exe) -type invent -trace > $@

gen/zork.trace.2: $(exe) src/*.hs Makefile .gen
	$(exe) -walk inputs2 -trace > $@

gen/zork.out1: $(exe) src/*.hs Makefile .gen
	$(exe) -walk inputs1 > $@

gen/zork.out2: $(exe) src/*.hs Makefile .gen
	$(exe) -walk inputs2 > $@


.gen:
	mkdir -p gen

$(exe): src/*.hs
	stack build; touch $(exe)
