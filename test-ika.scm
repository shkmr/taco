(use gauche.test)
(use vmhack)
;; (add-load-path ".") (add-load-path "./vmhack")
(test-start "ika")
(use ika)
(test-module 'ika)

(define test-code1 '(%top-level (0 0)
                         (CONST) 2
                         (RET)
                         ))

(define test-code2 '(%top-level (0 0)
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
  '(%top-level (0 0)
        (CLOSURE) (hello (0 0)
            (CONST-PUSH) "hello, world"
            (GREF-TAIL-CALL 1) (mkid print)
            (RET))
        (DEFINE 0) (mkid hello)
        (RET)
        ))

(define test-code4
  '(%top-level (0 0)
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
  '(%top-level (0 0)
               (CLOSURE) (fo (1 0)
                             (LREF0)
                             (BNEQC) fobar (label 1)
                             (CONST-RET) yes
                             (label 1)
                             (CONST-RET) no)
               (DEFINE 0) (mkid fo)
               (RET)
               ))

(define (a name code)
  (display "*****: ")
  (print name)
  (ika/pp code)
  (print "====> ")
  (let ((cc (ika->vm-code code)))
    (ika/pp (append '(%top-level (0 0))
                    (vm-code->list cc)))
    (vm-dump-code cc)))

(a "test-code1" test-code1)
(a "test-code2" test-code2)
(a "test-code3" test-code3)
(a "test-code4" test-code4)
(a "test-code5" test-code5)
(newline)

(test* "const-ret"             2     (vm-code-execute! (ika->vm-code test-code1) (interaction-environment)))
(test* "(1+2)*(3+4)"          21     (vm-code-execute! (ika->vm-code test-code2) (interaction-environment)))
(test* "(define (hello) ...)" 'hello (vm-code-execute! (ika->vm-code test-code3) (interaction-environment)))
(test* "(hello)" "hello, world\n" (with-output-to-string hello))
(test* "(define (fact n) ...)" 'fact (vm-code-execute! (ika->vm-code test-code4) (interaction-environment)))
(test* "(fact 5)" 120 (fact 5))
(test* "(define (fo x) ...)" 'fo     (vm-code-execute! (ika->vm-code test-code5) (interaction-environment)))
(test* "(fo 'fobar)" 'yes (fo 'fobar))
(test* "(fo 'fobaz)" 'no  (fo 'fobaz))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF

