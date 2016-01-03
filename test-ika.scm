(use gauche.test)
(use vmhack)
(test-start "ika")
(use ika)
(test-module 'ika)

(define test-code1
  '(%toplevel (0 0)
        (CONST) 2
        (RET)
        ))

(define test-code2
  '(%toplevel (0 0)
        (CONST) 1
        (PUSH)
        (CONST) 2
        (NUMADD2)
        (PUSH)
        (CONST) 3
        (PUSH)
        (CONST) 4
        (NUMADD2)
        (NUMMUL2)
        (RET)
        ))

(define test-code3
  '(%toplevel (0 0)
        (CLOSURE) (hello (0 0)
            (CONST-PUSH) "hello, world"
            (GREF-TAIL-CALL 1) (mkid print)
            (RET))
        (DEFINE 0) (mkid hello)
        (RET)
        ))

(define test-code4
  '(%toplevel (0 0)
        (CLOSURE) (FACT (1 0)
                      (LREF0)
                      (BNUMNEI 1) (label 1)
                      (CONST) 1
                      (RET)
                      (label 1)
                      (LREF0-PUSH)
                      (PRE-CALL 1) (label 2)
                      (LREF0-NUMADDI-PUSH -1)
                      (GREF-CALL 1) (mkid fact)
                      (label 2)
                      (NUMMUL2)
                      (RET))
        (DEFINE 0) (mkid fact)
        (RET)
        ))

(define test-code5
  '(%toplevel (0 0)
        (CLOSURE) (fo (1 0)
                      (LREF0)
                      (BNEQC) fobar (label 1)
                      (CONST-RET) yes
                      (label 1)
                      (CONST-RET) no)
        (DEFINE 0) (mkid fo)
        (RET)
        ))

(define test-code6
  '(%toplevel (0 0)
        (info "(lambda (x) (case x ((fobar) 'yes) (else 'no)))")
        (CLOSURE) (fo (1 0)
                      (info "x")
                      (LREF0)
                      (BNEQC) fobar (label 1)
                      (CONST-RET) yes
                      (label 1)
                      (CONST-RET) no)
        (info "(define (fo x) ...)")
        (DEFINE 0) (mkid fo)
        (RET)
        ))

;;; ToDo: write test case for ``codes'' operand type.

(define (a name code)
  (print "*** " name " **************************************")
  (ika/pp code)
  #;(print "====> ")
  (let ((cc (ika->vm-code code)))
    #;(ika/pp (append '(%toplevel (0 0))
                    (vm-code->list cc)))
    (vm-dump-code cc)))

(a "test-code1" test-code1)
(a "test-code2" test-code2)
(a "test-code3" test-code3)
(a "test-code4" test-code4)
(a "test-code5" test-code5)
(a "test-code6" test-code6)
(newline)

(define (run code) (vm-code-execute! (ika->vm-code code) (interaction-environment)))

(test* "const-ret"             2     (run test-code1))
(test* "(1+2)*(3+4)"          21     (run test-code2))
(test* "(define (hello) ...)" 'hello (run test-code3))
(test* "(hello)" "hello, world\n" (with-output-to-string hello))
(test* "(define (fact n) ...)" 'fact (run test-code4))
(test* "(fact 5)" 120 (fact 5))
(test* "(define (fo x) ...)" 'fo     (run test-code5))
(test* "(fo 'fobar)" 'yes (fo 'fobar))
(test* "(fo 'fobaz)" 'no  (fo 'fobaz))
(test* "info test" 'fo               (run test-code6))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF

