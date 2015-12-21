;;;
;;; Test vmhack
;;;
(use gauche.test)
(use gauche.vm.insn)
(use gauche.vm.code)

(test-start "vmhack")
(use vmhack)
(test-module 'vmhack)

;;
(define-macro (import-from mod . syms)
  `(begin
     ,@(map (lambda (sym) `(define ,sym (with-module ,mod ,sym)))
            syms)))
(import-from gauche.internal compile)

(define (cpr sexp) ; compile-print-run
  (let ((code (compile sexp (interaction-environment))))
    (newline)
    (print "=== input: " sexp)
    (vm-dump-code code)
    (print "=== list:" (vm-code->list code))
    (vm-code-execute! code (interaction-environment))))

(define (test-fact n)
  (let ((prog '(define (fact n)
                 (if (= n 1)
                   1
                   (* n (fact (- n 1)))))))
    (cpr prog)
    (cpr `(fact ,n))))

(define (test-const-ret n)
  (let ((ccb (make-compiled-code-builder 0 0 '%toplevel #f #f #f))
        (maxstack 0))
    (compiled-code-emit0o! ccb (vm-insn-name->code 'CONST) n)
    (compiled-code-emit-RET! ccb)
    (compiled-code-finish-builder ccb maxstack)
    #;(vm-code->list ccb)
    #;(vm-dump-code ccb)
    (vm-code-execute! ccb (interaction-environment))))

(test* "(fact 5)" 120 (test-fact 5))
(test* "const-ret" 2 (test-const-ret 2))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF
