;;;
;;;  ika : IKAssembler for TACOmpiler
;;;
(define-module ika
  (use gauche.vm.insn)
  (use gauche.vm.code)
  (use vmhack)
  (export ika/pp
          ika->vm-code
          vm-dump-code
          vm-code->list
          sexp->list
          compile
          compile-p1
          compile-p2
          compile-p3
          compile-p4
          compile-p5
          ))
(select-module ika)

;;;
;;; (not quite) pretty print ika source code
;;;
(define (ika/pp prog)
  (define (ff code level)
    (let ((sp (make-string level #\space)))
      (for-each (lambda (n e)
                  (format #t "~va~3,'0d: ~a~%" level "" n e)
                  (if (is-a? e <compiled-code>)
                    (ff (vm-code->list e) (+ level 4))))
                (iota (length code))
                code)))
  (newline)
  (ff prog 0))

(define (ika/pp2 prog)
  (define (ff code level)
    (let ((sp (make-string level #\space)))

      (define (disp obj) (display sp) (display obj) (newline))
      (define (wri  obj) (display sp) (write obj) (newline))

      (for-each (lambda (elm)
                  (cond ((and (pair? elm)
                              (symbol? (car elm)))
                         (disp elm))
                        ((and (pair? elm)
                              (not (symbol? (car elm))))
                         (disp "(") 
                         (ff elm (+ level 4)) 
                         (disp ")"))
                        (else
                         (wri elm))))
                code)))
  (display "(") (newline)
  (ff prog 4)
  (display ")") (newline))


;;;
;;; import useful stuff
;;;
(define-macro (import-from mod . syms)
  `(begin
     ,@(map (lambda (sym) `(define ,sym (with-module ,mod ,sym)))
            syms)))

(import-from gauche.internal
             compile
             compile-p1
             compile-p2
             compile-p3
             compile-p4
             compile-p5)

(import-from gauche <compiled-code>)

;;;
;;;
;;;
(define (c sexp)
  (let ((cc (compile sexp (interaction-environment))))
    (ika/pp (vm-code->list cc))
    #;(vm-dump-code cc)))

;;;
;;;
;;;
(define (ika->vm-code-old ika)
  (let ((ccb (make-compiled-code-builder 0 0 '%toplevel #f #f #f))
        (maxstack 0))
    (for-each (lambda (stmt)
                (case (car stmt)
                  ((CONST)
                   (and (= (length stmt) 2)
                        (integer? (cadr stmt))
                        (compiled-code-emit0o! ccb
                                               (vm-insn-name->code 'CONST)
                                               (cadr stmt))))
                  ((RET)
                   (compiled-code-emit-RET! ccb))
                  (else (error #"ika: unknown mnemonic: ~(car stmt)"))))
              ika)
    (compiled-code-finish-builder ccb maxstack)
    ccb))

(define (ika->vm-code ika)
  (let ((ccb (make-compiled-code-builder 0 0 '%toplevel #f #f #f))
        (maxstack 0))
    (for-each (lambda (stmt)
                (if (pair? stmt)
                  (case (length stmt)
                    ((1) (compiled-code-emit0!  ccb
                                                (vm-insn-name->code (car stmt))))
                    ((2) (compiled-code-emit0o! ccb
                                                (vm-insn-name->code (car stmt))
                                                (cadr stmt)))
                    ((3)
                    (else (error "ika: wrong format: " stmt)))
                  (error "ika: object is not supported yet")))
              ika)
    (compiled-code-finish-builder ccb maxstack)
    ccb))

#|
(vm-insn-code 'CONST)
(vm-insn-name->code 'CONST)
(vm-insn-code->name 2)
(disasm fact)
(ika/pp (vm-code->list (closure-code fact)))

(c '(+ 1 2))
(c '(lambda (a b c) (fobar a b c)))

(c '(define (fact n)
      (if (= n 1)
        1
        (* n (fact (- n 1))))))
|#

;; EOF
