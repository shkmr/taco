(use gauche.test)
(use vmhack)

(test-start "ika")
#|
(beign
 (add-load-path ".")
 (add-load-path "./vmhack"))
 (load "ika.scm")
 (use file.util)
|#
(use ika)
(test-module 'ika)

(define test-code '((CONST 2) (RET)))
(define scm-fact  '(define (fact n)
                     (if (= n 1)
                         1
                         (* n (fact (- n 1))))))


(vm-code-execute (compile scm-fact (interaction-environment))
                 (interaction-environment))

(print #"(fact 5) => ~(fact 5)")

(print (vm-code->list (ika->vm-code test-code)))

(test* "const-ret" 2 (vm-code-execute (ika->vm-code test-code)
                                      (interaction-environment)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF

