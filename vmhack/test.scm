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

(define test-code1
  '(define (fact n)
     (if (= n 1)
       1
       (* n (fact (- n 1))))))

(test* "(define (fact n) ...)" 'fact (cpr test-code1))
(test* "(fact 5)"              120   (fact 5))
(test* "(fact 6)"              720   (cpr '(fact 6)))

;; ToDo: add tests for compiled-code-*

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF
