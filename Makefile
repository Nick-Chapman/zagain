
top: trace walk dis code diff


dev: niz.walk my.walk
	git diff --no-index niz.walk my.walk

STORY = ~/z/story/trinity.12-860926.z4
SCRIPT = trinity.script.long
LEN = 100000


my.walk: $(exe) $(SCRIPT).me Makefile src/*.hs
	stack build
	bash -c '($(exe) $(STORY) -walk $(SCRIPT).me -nodebug | head -$(LEN) > my.walk 2>&1) || true'


niz.walk: $(SCRIPT) Makefile
	cat $(SCRIPT) | niz -no-line-wrap -hide-unimplemented $(STORY) | sed 's/  *(/ (/g' | sed 's/  *"/ "/g'| head -$(LEN) > niz.walk


exe = .stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/main.exe/main.exe

trace: .reg reg/zork.trace
walk: .reg reg/zork.walk reg/h.walk reg/judo.walk reg/trinity.walk
dis: .reg reg/zork.dis reg/h.dis reg/judo.dis reg/trinity.dis
code: .reg reg/zork.code reg/h.code

diff:
	git diff reg

reg/zork.trace: $(exe) z.script src/*.hs
	$(exe) -nodebug -trace -type 'open mailbox' -type 'read leaflet' > $@

reg/zork.walk: $(exe) z.script src/*.hs
	$(exe) -nodebug -walk z.script > $@

reg/zork.dis: $(exe) src/*.hs
	$(exe) dis -walk z.script > $@

reg/zork.code: $(exe) src/*.hs
	$(exe) comp -nodebug > $@

reg/h.walk: $(exe) h.script src/*.hs
	$(exe) -nodebug story/hitchhiker-r59-s851108.z3 -walk h.script > $@

reg/h.dis: $(exe) src/*.hs
	$(exe) dis story/hitchhiker-r59-s851108.z3 -walk h.script > $@

reg/h.code: $(exe) src/*.hs
	$(exe) comp -nodebug story/hitchhiker-r59-s851108.z3 > $@

reg/judo.dis: $(exe) src/*.hs
	$(exe) dis story/judo-night.1-080706.z5 > $@

reg/judo.walk: $(exe) h.script src/*.hs
	$(exe) -nodebug story/judo-night.1-080706.z5 -walk j.script.long > $@

reg/trinity.dis: $(exe) src/*.hs
	$(exe) dis story/trinity.12-860926.z4 > $@

reg/trinity.walk: $(exe) trinity.script src/*.hs
	$(exe) -nodebug story/trinity.12-860926.z4 -walk trinity.script > $@

.reg:
	mkdir -p reg

$(exe): src/*.hs
	stack build; touch $(exe)
