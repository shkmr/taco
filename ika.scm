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
          make-identifier
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
             make-identifier
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
                (format #t "~va~4,' d ~s~%" level "" n (car code))
                (ff (cdr code) level (+ n 1)))
               ((code)
                (cond ((is-a? (cadr code) <compiled-code>)
                       (format #t "~va~3,' d ~s ~s~%" level "" n (car code) (cadr code))
                       (ff (vm-code->list (cadr code)) (+ level 4) 0))
                      ((and (pair? (cadr code))
                            (symbol? (caadr code)))
                       (format #t "~va~4,' d ~s (~s ~s~%" level ""
                               n (car code) (caadr code) (cadadr code))
                       (ff (cddadr code) (+ level 4) 0))
                      (else
                       (format #t "~va~3,'d ~s ~s~%" level "" n (car code) (cadr code))))
                (ff (cddr code) level (+ n 2)))
               (else
                (format #t "~va~4,' d ~s ~s~%" level "" n (car code) (cadr code))
                (ff (cddr code) level (+ n 2))))))
          (else
           (error "ika: syntax error:" code))))
  (format #t "(~s ~s~%" (car prog) (cadr prog))
  (ff (cddr prog) 4 0))

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
    (newline)
    (ika/pp (append '(%top-level (0 0))
                    (vm-code->list cc)))
    (newline)
    (vm-dump-code cc)
    ))

;;;
;;;
;;;
(define (ika->vm-code ika)
  (let ((name     (car ika))
        (reqargs  (caadr ika))
        (optargs  (cadadr ika)))
    (let ((ccb (make-compiled-code-builder reqargs optargs name #f #f #f)))
      (let lp ((ika (cddr ika))
               (maxstack 0))
        (cond ((null? ika)
               (compiled-code-finish-builder ccb maxstack)
               ccb)
              ((pair? (car ika))
               (let ((np   (- (length (car ika)) 1))
                     (info (vm-find-insn-info (caar ika))))
                 (if (not (= np  (~ info'num-params)))
                   (error #"ika: wrong number of parameters, required ~(~ info'num-params), got ~|np|."))
                 (let ((arg0 (if (>= np 1) (cadar ika) 0))
                       (arg1 (if (= np 2) (caddar ika) 0)))

                   (case (~ info'operand-type)

                     ((none)
                      (compiled-code-emit2! ccb (~ info'code) arg0 arg1)
                      (lp (cdr ika) maxstack))

                     ((code)
                      (let ((operand (cadr ika)))
                        (if (pair? operand)
                          (compiled-code-emit2o! ccb (~ info'code) arg0 arg1 (ika->vm-code operand))
                          (compiled-code-emit2o! ccb (~ info'code) arg0 arg1 operand))
                        (lp (cddr ika) maxstack)))

                     ((obj)
                      (let ((operand (cadr ika)))
                        (if (and (pair? operand) (eq? 'mkid (car operand)))
                          (compiled-code-emit2o! ccb (~ info'code) arg0 arg1 (make-identifier (cadr operand) (find-module 'user) '()))
                          (compiled-code-emit2o! ccb (~ info'code) arg0 arg1 operand))
                        (lp (cddr ika) maxstack)))

                     (else
                      (let ((operand (cadr ika)))
                        (compiled-code-emit2o! ccb (~ info'code) arg0 arg1 operand)
                        (lp (cddr ika) maxstack)))

                     ))))

              (else
               (error "ika: syntax error")))))))

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
(c '(define (hello)
      (print "hello, world")))

<vm-insn-info>  vm-build-insn
vm-insn-size
(slot-ref (vm-find-insn-info 'CONST) 'num-params)
(slot-ref (vm-find-insn-info 'CONST) 'alt-num-params)
(slot-ref (vm-find-insn-info 'CONST) 'operand-type)
(make-identifier 'print (find-module 'user) '())
|#

;; EOF
