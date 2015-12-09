;;;
;;; Test vmhack
;;;
(use gauche.test)
(use gauche.vm.insn)
(use gauche.vm.code)
;;;
;;; from Gauche/src/compile.scm
;;;
(define-macro (define-insn-constants)
  (let1 name&codes
      (map (^[insn] (cons (car insn) (ref (cdr insn) 'code)))
           (class-slot-ref <vm-insn-info> 'all-insns))
    `(begin
       ,@(map (^[n&c] `(define-constant ,(car n&c) ,(cdr n&c)))
              name&codes)
       (define-constant .insn-alist. ',name&codes)
       )))
(define-insn-constants)

(test-start "vmhack")
(use vmhack)
(test-module 'vmhack)

(print "========= .insn-alist. ========")
(for-each print (reverse .insn-alist.))
(newline)

(define-macro (import-from mod . syms)
  `(begin
     ,@(map (lambda (sym) `(define ,sym (with-module ,mod ,sym)))
            syms)))
(import-from gauche.internal compile)

(define (compile&run sexp)
  (let ((code (compile sexp (interaction-environment))))
    (newline)
    (print "=== input: " sexp)
    (vm-dump-code code)
    (vm-code-execute! code (interaction-environment))))

(define (test-fact n)
  (let ((program '(define (fact n)
                    (if (= n 1)
                        1
                        (* n (fact (- n 1)))))))
    (compile&run program)
    (compile&run `(fact ,n))))


(define (test-const-ret n)
  (let ((ccb (make-compiled-code-builder 0 0 '%toplevel #f #f #f)))
    (compiled-code-emit0o! ccb CONST n)
    (compiled-code-emit-RET! ccb)
    (compiled-code-finish-builder ccb 1)
    #;(vm-code->list ccb)
    #;(vm-dump-code ccb)
    (vm-code-execute! ccb (interaction-environment))))

(test* "(fact 5)" 120 (test-fact 5))
(test* "const-ret" 2 (test-const-ret 2))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF
