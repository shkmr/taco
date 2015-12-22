(use gauche.test)
(use vmhack)
;; (add-load-path ".") (add-load-path "./vmhack")
(test-start "ika")
(use ika)
(test-module 'ika)

(define test-code1 '((CONST) 2 (RET)))

(define test-code2 '(
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

(define (a name code)
  (display "====: ")
  (display name)
  (ika/pp code)
  (display "====> ")
  (ika/pp (vm-code->list (ika->vm-code code))))

(a "test-code1" test-code1)

(a "test-code2" test-code2)

(test* "const-ret" 2 (vm-code-execute! (ika->vm-code test-code1)
                                       (interaction-environment)))

(test* "(1+2)*(3+4)" 21 (vm-code-execute! (ika->vm-code test-code2)
                                          (interaction-environment)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF

