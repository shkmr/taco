(use gauche.test)
(use vmhack)

(test-start "ika")
(use ika)
(test-module 'ika)

(define test-code '((CONST 2) (RET)))

(vm-dump-code (ika->vm-code test-code))
(print (vm-code->list (ika->vm-code test-code)))

(test* "const-ret" 2 (vm-code-execute (ika->vm-code test-code)
                                      (interaction-environment)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF

