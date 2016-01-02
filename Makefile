all : insn.html vmhack/vmhack.so

vmhack/vmhack.so :
	(cd vmhack; ./configure; make check)

insn.html : make-insn-table.scm
	gosh make-insn-table.scm > insn.html

mgvm.log :
	gosh -I. test-mgvm.scm v > mgvm.log

bench :
	gosh -Ilalr -I. -Ivmhack -ltaco3.scm test-bench.scm

check : check-taco3 check-taco2 check-tlex check-ika

check-taco3 :
	@gosh -Ilalr -I. -Ivmhack -ltaco3.scm test-taco3.scm > test-taco3.log

check-taco2 :
	@gosh -Ilalr -I. test-taco2.scm > test-taco2.log

check-tlex :
	@gosh -I.  test-tlex.scm > test-tlex.log

check-ika :
	@gosh -I. -Ivmhack test-ika.scm > test-ika.log

clean :
	rm -f *.log *~ insn.html taco0.out taco2.out

