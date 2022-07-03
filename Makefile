
top: trace walk dis code diff

exe = .stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/main.exe/main.exe

trace: .reg reg/zork.trace
walk: .reg reg/zork.walk reg/h.walk reg/judo.walk reg/trinity.walk
dis: .reg reg/zork.dis reg/h.dis reg/judo.dis reg/trinity.dis
code: .reg reg/zork.code reg/h.code reg/judo.code reg/trinity.code

diff:
	git diff reg

reg/zork.trace: $(exe) z.script src/*.hs
	$(exe) -nodebug zork -trace -type 'open mailbox' -type 'read leaflet' > $@

reg/zork.walk: $(exe) z.script src/*.hs
	$(exe) -nodebug zork -walk z.script > $@

reg/zork.dis: $(exe) src/*.hs
	$(exe) dis zork -walk z.script > $@

reg/zork.code: $(exe) src/*.hs
	$(exe) code zork -nodebug > $@

reg/h.walk: $(exe) h.script src/*.hs
	$(exe) -nodebug hitch -walk h.script > $@

reg/h.dis: $(exe) src/*.hs
	$(exe) dis hitch -walk h.script > $@

reg/h.code: $(exe) src/*.hs
	$(exe) code -nodebug hitch > $@

reg/judo.dis: $(exe) src/*.hs
	$(exe) dis judo > $@

reg/judo.walk: $(exe) h.script src/*.hs
	$(exe) -nodebug judo -walk j.script.long > $@

reg/judo.code: $(exe) src/*.hs
	$(exe) code -nodebug judo > $@

reg/trinity.dis: $(exe) src/*.hs
	$(exe) dis trinity > $@

reg/trinity.walk: $(exe) trinity.script src/*.hs
	$(exe) -nodebug trinity -walk trinity.script > $@

reg/trinity.code: $(exe) src/*.hs
	$(exe) code -nodebug trinity > $@

.reg:
	mkdir -p reg

$(exe): src/*.hs
	stack build; touch $(exe)
