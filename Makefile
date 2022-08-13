
top: trace walk dis code diff

exe = .stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/main.exe/main.exe


dev: gold.out dev.out
	git diff --no-index $^

gold.out: dev.out reg/zork.trace
	bash -c 'cat reg/zork.trace | head -$$(cat dev.out | wc -l) > $@'

dev.out: $(exe) Makefile
	bash -c '$(exe) zork -trace -walk scripts/zork.script -trace -viacomp > $@ 2>&1 || true'


trace: .reg reg/zork.trace
walk: .reg reg/zork.walk reg/h.walk reg/judo.walk reg/trinity.walk
dis: .reg reg/zork.dis reg/h.dis reg/judo.dis reg/trinity.dis
code: .reg reg/zork.code reg/h.code reg/judo.code reg/trinity.code

diff:
	git diff reg

reg/zork.trace: $(exe) scripts/zork.script src/*.hs Makefile
	bash -c '$(exe) -nodebug zork -trace -walk <(head -5 scripts/zork.script) -viacomp > $@'

reg/zork.walk: $(exe) scripts/zork.script src/*.hs Makefile
	$(exe) -nodebug zork -walk scripts/zork.script -viacomp > $@

reg/zork.dis: $(exe) src/*.hs
	$(exe) dis zork -walk scripts/zork.script > $@

reg/zork.code: $(exe) src/*.hs
	$(exe) code zork -nodebug > $@

reg/h.walk: $(exe) scripts/hitch.script src/*.hs
	$(exe) -nodebug hitch -walk scripts/hitch.script > $@

reg/h.dis: $(exe) src/*.hs
	$(exe) dis hitch -walk scripts/hitch.script > $@

reg/h.code: $(exe) src/*.hs
	$(exe) code -nodebug hitch > $@

reg/judo.dis: $(exe) src/*.hs
	$(exe) dis judo > $@

reg/judo.walk: $(exe) scripts/judo.script src/*.hs
	$(exe) -nodebug judo -walk scripts/judo.script > $@

reg/judo.code: $(exe) src/*.hs
	$(exe) code -nodebug judo > $@

reg/trinity.dis: $(exe) src/*.hs
	$(exe) dis trinity > $@

reg/trinity.walk: $(exe) scripts/trinity.script src/*.hs
	$(exe) -nodebug trinity -walk scripts/trinity.script > $@

reg/trinity.code: $(exe) src/*.hs
	$(exe) code -nodebug trinity > $@

.reg:
	mkdir -p reg

$(exe): src/*.hs
	stack build; touch $(exe)
