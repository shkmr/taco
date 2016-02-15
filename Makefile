all : insn.html vmhack/vmhack.so

demoq :
	gosh -Ivmhack -I. -ltaco3.scm taco3 test.taco

demov :
	gosh -Ivmhack -I. -ltaco3.scm taco3 -v test.taco

vmhack/vmhack.so :
	(cd vmhack; ./configure; make check)

insn.html : make-insn-table.scm
	gosh make-insn-table.scm > insn.html

mgvm.log :
	gosh -I. test-mgvm.scm v > mgvm.log

bench :
	gosh  -I. -Ivmhack -ltaco3.scm test-bench.scm

check : check-taco3 check-taco2 check-tlex check-ika

check-taco3 :
	@gosh -I. -Ivmhack -ltaco3.scm test-taco3.scm > test-taco3.log

check-taco2 :
	@gosh -I. test-taco2.scm > test-taco2.log

check-tlex :
	@gosh -I.  test-tlex.scm > test-tlex.log

check-ika :
	@gosh -I. -Ivmhack test-ika.scm > test-ika.log

clean :
	rm -f *.log *~ insn.html taco0.out taco2.out


install : all
	@echo "There is nothing worth to install..."

ika-intro.txt : ika-intro.scm
	gpsh ika-intro.scm > ika-intro.txt

INSTALL      = "/usr/local/bin/gauche-install" -C
TACOIKAFILES = taco3.scm tlex.scm ika.scm
LALRFILES    = lalr/lalr-gauche.scm lalr/lalr.scm
install-anyway :
	(cd vmhack; make install-anyway)
	$(INSTALL) -m 444 -T `gauche-config --sitelibdir` $(TACOIKAFILES)
	$(INSTALL) -m 444 -T `gauche-config --sitelibdir` -p lalr $(LALRFILES)
	$(INSTALL) -m 755 -T `gauche-config --prefix`/bin taco3
