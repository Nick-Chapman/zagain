
top: dev reg

#WRAP = -wrap 80
#dev: compare_with_frotz
dev: compare_with_mojo

LEN = 999
STORY = ./story/zork1.88-840726.z3
SCRIPT = z.script

#LEN = 999
#STORY = ./story/hitchhiker-r59-s851108.z3
#SCRIPT = h.transcript

exe = .stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/main.exe/main.exe

compare_with_mojo: my.walk mojo.walk
	git diff --no-index my.walk mojo.walk

compare_with_frotz: my.walk frotz.walk
	git diff --no-index my.walk frotz.walk

my.walk: $(exe) $(SCRIPT) Makefile
	bash -c '($(exe) $(STORY) $(WRAP) -walk <(head -$(LEN) $(SCRIPT)) > my.walk 2>&1) || true'


frotz.walk: dfrotz $(SCRIPT) Makefile
	cat $(SCRIPT) | head -$(LEN) | ~/code/other/frotz/dfrotz -h 99 -w 80 $(STORY) > frotz.walk || true

dfrotz:
	(cd ~/code/other/frotz; make dumb)


mojo.walk: ~/code/other/mojozork/mojozork.exe $(SCRIPT)
	~/code/other/mojozork/mojozork.exe $(STORY) $(SCRIPT) > mojo.walk

~/code/other/mojozork/mojozork.exe: ~/code/other/mojozork/mojozork.c
	(cd ~/code/other/mojozork; make)


# regression...

reg: .gen gen/zork.dis gen/zork.trace gen/zork.walk gen/h.walk
	git diff gen

gen/zork.dis: $(exe) src/*.hs
	$(exe) dis > $@

# trace just the first 2 steps of the zork walk-though
gen/zork.trace: $(exe) z.script src/*.hs
	bash -c '$(exe) -nodebug -trace -walk <(head -2 z.script) > $@'

gen/zork.walk: $(exe) z.script src/*.hs
	bash -c '$(exe) -nodebug -walk z.script > $@'

gen/h.walk: $(exe) h.script src/*.hs
	bash -c '$(exe) story/hitchhiker-r59-s851108.z3 -nodebug -walk h.script > $@'


.gen:
	mkdir -p gen

$(exe): src/*.hs
	stack build; touch $(exe)
