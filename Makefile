
top: reg trace walk

# regression tests...

reg: gen/zork.dis gen/zork.objects

gen/zork.dis: src/*.hs Makefile .gen
	stack run dis > $@

gen/zork.objects: src/*.hs Makefile .gen
	stack run objects > $@


trace: gen/zork.trace gen/zork.trace.invent gen/zork.trace.2 #gen/zork.trace.jump

gen/zork.trace: story/zork1.88-840726.z3 run.sh src/*.hs Makefile .gen
	cat /dev/null | ./run.sh -trace $< > $@

gen/zork.trace.invent: story/zork1.88-840726.z3 run.sh src/*.hs Makefile .gen
	echo invent | ./run.sh -trace $< > $@

gen/zork.trace.2: story/zork1.88-840726.z3 run.sh src/*.hs Makefile .gen
	#cat inputs2 | ./run.sh -trace $< > $@
	stack run -- reg -trace $< 'open mailbox' 'read leaflet' > $@

# for "jump", we dont yet match old niz
gen/zork.trace.jump: story/zork1.88-840726.z3 run.sh src/*.hs Makefile .gen
	echo jump | ./run.sh -trace $< > $@


walk: gen/zork.out1 gen/zork.out2

gen/zork.out1: story/zork1.88-840726.z3 run.sh src/*.hs Makefile .gen
	cat inputs1 | ./run.sh $< > $@

gen/zork.out2: story/zork1.88-840726.z3 run.sh src/*.hs Makefile .gen
	#cat inputs2 | ./run.sh $< > $@
	stack run -- reg $< 'open mailbox' 'read leaflet' > $@


.gen:
	mkdir -p gen
