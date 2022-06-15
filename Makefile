
top: trace walk dis code diff

exe = .stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/main.exe/main.exe

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
