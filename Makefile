
top: reg dev

exe = .stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/main.exe/main.exe

dev: my.walk mojo.walk Makefile
	git diff --no-index my.walk mojo.walk

my.walk: $(exe) z.script Makefile
	bash -c '($(exe) -walk <(tail +2 z.script) -nobuf > my.walk 2>&1) || true'

mojo.walk: .mojo z.script Makefile
	~/code/other/mojozork/mojozork.exe ./story/zork1.88-840726.z3 z.script > mojo.walk

.mojo: Makefile
	(cd ~/code/other/mojozork; make)


# regression...

reg: .gen gen/zork.dis gen/zork.trace gen/zork.walk
	git diff gen

gen/zork.dis: $(exe) src/*.hs Makefile
	$(exe) dis > $@

# trace just the first 2 steps of the zork walk-though
gen/zork.trace: $(exe) z.script src/*.hs Makefile
	bash -c '$(exe) -nodebug -trace -walk <(tail +2 z.script | head -2) > $@'

# run the zork walk-though as far as we can before crashing
gen/zork.walk: $(exe) z.script src/*.hs Makefile
	bash -c '$(exe) -nodebug -walk <(tail +2 z.script) > $@'


.gen:
	mkdir -p gen

$(exe): src/*.hs
	stack build; touch $(exe)
