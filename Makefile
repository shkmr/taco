all : insn.html

insn.html : make-insn-table.scm
	gosh make-insn-table.scm > insn.html

mgvm.log :
	gosh -I. test-mgvm.scm v > mgvm.log

check : check-taco2 check-tlex check-ika check-taco0

check-taco2 :
	@gosh -Ilalr -I. test-taco2.scm > test-taco2.log

check-tlex :
	@gosh -I.  test-tlex.scm > test-tlex.log

check-ika :
	@gosh -I. -Ivmhack test-ika.scm > test-ika.log

check-taco0 :
	@gosh -I. -Ilalr -Ivmhack test-taco0.scm > test-taco0.log

clean :
	rm -f *.log *~ insn.html taco0.out taco2.out

