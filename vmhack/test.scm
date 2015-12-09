;;;
;;; Test vmhack
;;;
(use gauche.test)
(test-start "vmhack")
(use vmhack)
(test-module 'vmhack)

(define (test-const-ret n)
  (let ((ccb (make-compiled-code-builder 0 0 '%toplevel #f #f #f)))
    (compiled-code-emit0o! ccb CONST n)
    (compiled-code-emit-RET! ccb)
    (compiled-code-finish-builder ccb 1)
    #;(vm-code->list ccb)
    #;(vm-dump-code ccb)
    (vm-code-execute ccb (interaction-environment))))

(test* "const-ret" 2 (test-const-ret 2))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF
