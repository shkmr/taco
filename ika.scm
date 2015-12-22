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
          compile
          compile-p1
          compile-p2
          compile-p3
          compile-p4
          compile-p5
          ))
(select-module ika)

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

;;;
;;; (not quite) pretty print ika source code
;;;
(define (ika/pp prog)
  (define (ff code level n)
    (cond ((null? code) #t)
          ((pair? (car code))
           (let ((info (vm-find-insn-info (caar code))))
             (case (~ info'operand-type)
               ((none)
                (format #t "~va~3,'0d: ~a~%" level "" n (car code))
                (ff (cdr code) level (+ n 1)))
               (else
                (format #t "~va~3,'0d: ~a ~a~%" level ""
                        n (car code) (cadr code))
                (if (is-a? (cadr code) <compiled-code>)
                  (ff (vm-code->list (cadr code)) (+ level 4) 0))
                (ff (cddr code) level (+ n 2))))))
          (else
           (error "ika: syntax error:" code))))
  (newline)
  (ff prog 0 0))

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
;;;
;;;
(define (c sexp)
  (let ((cc (compile sexp (interaction-environment))))
    (ika/pp (vm-code->list cc))
    #;(vm-dump-code cc)
    ))
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
  (let ((ccb (make-compiled-code-builder 0 0 '%toplevel #f #f #f)))
    (let lp ((ika    ika)
             (maxstack 4))
      (cond ((null? ika)
             (compiled-code-finish-builder ccb maxstack)
             ccb)
            ((pair? (car ika))
             (let ((np   (- (length (car ika)) 1))
                   (info (vm-find-insn-info (caar ika))))
               (if (not (= np  (~ info'num-params)))
                 (error "ika: wrong number of parameters, got: " np))
               (case (~ info'operand-type)
                 ((none)
                  (compiled-code-emit0!  ccb (vm-insn-name->code (caar ika)))
                  (lp (cdr ika) maxstack))
                 (else
                  (compiled-code-emit0o! ccb
                                         (vm-insn-name->code (caar ika))
                                         (cadr ika))
                  (lp (cddr ika) maxstack)))))
            (else
             (error "ika: syntax error"))))))
        
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
<vm-insn-info>  vm-build-insn
vm-insn-size
(slot-ref (vm-find-insn-info 'CONST) 'num-params)
(slot-ref (vm-find-insn-info 'CONST) 'alt-num-params)
(slot-ref (vm-find-insn-info 'CONST) 'operand-type)

|#

;; EOF
