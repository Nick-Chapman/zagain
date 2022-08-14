
top: trace walk dis code diff

trace: .reg reg/zork.trace reg/trinity.trace
walk: .reg reg/zork.walk reg/hitch.walk reg/judo.walk reg/trinity.walk
dis: .reg reg/zork.dis reg/hitch.dis reg/judo.dis reg/trinity.dis
code: .reg reg/zork.code reg/hitch.code reg/judo.code reg/trinity.code

diff:
	git diff reg

.reg:
	mkdir -p reg

exe = .stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/main.exe/main.exe

$(exe): src/*.hs
	stack build; touch $(exe)


reg/zork.trace: $(exe) scripts/zork.script src/*.hs
	bash -c '$(exe) -nodebug zork -trace -walk <(head -5 scripts/zork.script) -viacomp > $@'

reg/trinity.trace: $(exe) scripts/trinity.script src/*.hs
	bash -c '$(exe) -nodebug trinity -trace -walk <(head -17 scripts/trinity.script) > $@'


reg/zork.walk: $(exe) scripts/zork.script src/*.hs
	$(exe) -nodebug zork -walk scripts/zork.script -viacomp > $@

reg/hitch.walk: $(exe) scripts/hitch.script src/*.hs
	$(exe) -nodebug hitch -walk scripts/hitch.script -viacomp > $@

reg/trinity.walk: $(exe) scripts/trinity.script src/*.hs
	$(exe) -nodebug trinity -walk scripts/trinity.script -viacomp > $@

reg/judo.walk: $(exe) scripts/judo.script src/*.hs
	$(exe) -nodebug judo -walk scripts/judo.script > $@


reg/zork.dis: $(exe) src/*.hs
	$(exe) dis zork -walk scripts/zork.script > $@

reg/hitch.dis: $(exe) src/*.hs
	$(exe) dis hitch -walk scripts/hitch.script > $@

reg/trinity.dis: $(exe) src/*.hs
	$(exe) dis trinity > $@

reg/judo.dis: $(exe) src/*.hs
	$(exe) dis judo > $@


reg/zork.code: $(exe) src/*.hs
	$(exe) code zork -nodebug > $@

reg/hitch.code: $(exe) src/*.hs
	$(exe) code -nodebug hitch > $@

reg/trinity.code: $(exe) src/*.hs
	$(exe) code -nodebug trinity > $@

reg/judo.code: $(exe) src/*.hs
	$(exe) code -nodebug judo > $@



dev: gold.out dev.out
	git diff --no-index $^

gold.out: dev.out reg/trinity.trace
	bash -c 'cat reg/trinity.trace | head -$$(cat dev.out | wc -l) > $@'

dev.out: $(exe) Makefile
	bash -c '$(exe) trinity -nodebug -trace -walk scripts/trinity.script -trace -viacomp > $@ 2>&1 || true'
