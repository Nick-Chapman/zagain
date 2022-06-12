
top: reg dev

LEN = 999
WIDTH = 90

#STORY = ./story/zork1.88-840726.z3
#SCRIPT = z.script

STORY = ./story/hitchhiker-r59-s851108.z3
SCRIPT = h.script

#dev: compare_with_frotz
#WRAP = -wrap $(WIDTH)

dev: compare_with_mojo
WRAP =

exe = .stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/main.exe/main.exe

compare_with_mojo: my.walk mojo.walk
	git diff --no-index my.walk mojo.walk

compare_with_frotz: my.walk frotz.walk
	git diff --no-index my.walk frotz.walk

my.walk: $(exe) $(SCRIPT) Makefile
	bash -c '($(exe) $(STORY) $(WRAP) -nodebug -walk <(head -$(LEN) $(SCRIPT)) > my.walk 2>&1) || true'


frotz.walk: dfrotz $(SCRIPT) Makefile
	cat $(SCRIPT) | head -$(LEN) | ~/code/other/frotz/dfrotz -q -h 99 -w $(WIDTH) $(STORY) > frotz.walk || true

dfrotz:
	(cd ~/code/other/frotz; make dumb)


mojo.walk: ~/code/other/mojozork/mojozork.exe $(SCRIPT) Makefile
	~/code/other/mojozork/mojozork.exe $(STORY) $(SCRIPT) > mojo.walk

~/code/other/mojozork/mojozork.exe: ~/code/other/mojozork/mojozork.c
	(cd ~/code/other/mojozork; make)


# regression...

reg: trace walk dis code diff

trace: .reg reg/zork.trace
walk: .reg reg/zork.walk reg/h.walk
dis: .reg reg/zork.dis reg/h.dis
code: .reg reg/zork.code reg/h.code

diff:
	git diff reg

reg/zork.trace: $(exe) z.script src/*.hs Makefile
	bash -c '$(exe) -nodebug -trace -walk <(head -2 z.script) > $@'

reg/zork.walk: $(exe) z.script src/*.hs Makefile
	$(exe) -walk z.script > $@

reg/zork.dis: $(exe) src/*.hs Makefile
	$(exe) dis -walk z.script > $@

reg/zork.code: $(exe) src/*.hs Makefile
	$(exe) comp > $@

reg/h.walk: $(exe) h.script src/*.hs Makefile
	$(exe) story/hitchhiker-r59-s851108.z3 -walk h.script > $@

reg/h.dis: $(exe) src/*.hs Makefile
	$(exe) dis story/hitchhiker-r59-s851108.z3 -walk h.script > $@

reg/h.code: $(exe) src/*.hs Makefile
	$(exe) comp story/hitchhiker-r59-s851108.z3 > $@


.reg:
	mkdir -p reg

$(exe): src/*.hs
	stack build; touch $(exe)
