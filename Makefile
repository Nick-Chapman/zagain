
top: trace walk dis code diff


judo: frotz.walk my.walk
	git diff --no-index frotz.walk my.walk

STORY = ~/z/story/judo-night.1-080706.z5
SCRIPT = j.script

my.walk: $(exe) $(SCRIPT) Makefile src/*.hs
	bash -c 'stack run -- $(STORY) -wrap 80 -walk <(head -20 j.script) > my.walk'

frotz.walk: $(SCRIPT) Makefile
	(echo 'k'; cat $(SCRIPT)) | ~/code/other/frotz/dfrotz $(STORY) > frotz.walk


exe = .stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/main.exe/main.exe

trace: .reg reg/zork.trace
walk: .reg reg/zork.walk reg/h.walk
dis: .reg reg/zork.dis reg/h.dis
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

.reg:
	mkdir -p reg

$(exe): src/*.hs
	stack build; touch $(exe)
