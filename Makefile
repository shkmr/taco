all : insn.html

insn.html : make-insn-table.scm
	gosh make-insn-table.scm > insn.html

check : check-ika check-taco0

check-ika :
	@gosh -I. -Ivmhack test-ika.scm > test-ika.log

check-taco0 :
	@gosh -I. -Ilalr -Ivmhack test-taco0.scm > test-taco0.log

clean :
	rm -f *.log *~ insn.html taco0.out

