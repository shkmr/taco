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
;;; pretty print ika program.
;;;
(define (ika/pp prog)
  (define (ff cp level n)
    (cond ((null? cp) #t)

          ;; pseudo insn. just label for now.
          ((and (pair? (car cp))
                (eq? (caar cp) 'label))
           (format #t "~va     ~s~%" level "" (car cp))
           (ff (cdr cp) level n))

          ;; VM insn.
          ((pair? (car cp))
           (let ((info (vm-find-insn-info (caar cp))))
             (case (~ info'operand-type)

               ((none)
                (format #t "~va~4,' d ~s~%" level "" n (car cp))
                (ff (cdr cp) level (+ n 1)))

               ((code)
                (let ((opcode  (car cp))
                      (operand (cadr cp)))
                  (cond ((is-a? operand <compiled-code>)
                         (format #t "~va~4,' d ~s ~s~%" level "" n opcode operand)
                         (ff (vm-code->list operand) (+ level 4) 0))
                        ((and (pair? operand) (symbol? (car operand)))
                         (format #t "~va~4,' d ~s (~s ~s~%" level "" n opcode (car operand) (cadr operand))
                         (ff (cddr operand) (+ level 4) 0)
                         (format #t "~va     )~%" level ""))
                        (else
                         (error "ika: operand has to be <compiled-code> or ika program, but got " operand)))
                  (ff (cddr cp) level (+ n 2))))

               ((codes)
                (let ((opcode  (car cp))
                      (operand (cadr cp)))
                  (if (not (pair? operand))
                    (error "ika: operand has to be list of codes, but got " operand))
                  (cond ((and (pair? (car operand)) (symbol? (caar operand)))
                         (format #t "~va~4,' d ~s (~%" level "" n opcode)
                         (for-each (lambda (prog)
                                     (format #t "~va     (~s ~s~%" level "" (car prog) (cadr prog))
                                     (ff (cddr prog) (+ level 4) 0)
                                     (format #t "~va)~%" level ""))
                                   operand)
                         (format #t "~va     )~%" level ""))

                        (else
                         (format #t "~va~4,' d ~s (~%" level "" n opcode)
                         (for-each (lambda (cc)
                                     (format #t "~va       ~s~%" level "" cc)
                                     (if (is-a? cc <compiled-code>)
                                       (ff (vm-code->list cc) (+ level 4) 0)))
                                   operand)
                         (format #t "~va     )~%" level "")))))

               ((obj+addr)
                (let ((opcode (car cp))
                      (obj    (cadr cp))
                      (addr   (caddr cp)))
                  (format #t "~va~4,' d ~s ~s ~s~%" level "" n opcode obj addr)
                  (ff (cdddr cp) level (+ n 3))))

               (else
                (format #t "~va~4,' d ~s ~s~%" level "" n (car cp) (cadr cp))
                (ff (cddr cp) level (+ n 2))))))
          (else
           (error "ika: syntax error:" cp))))
  (format #t "(~s ~s~%" (car prog) (cadr prog))
  (ff (cddr prog) 4 0))

;;;
;;;
;;;
(define (ika->vm-code ika)
  (let ((name     (car ika))
        (reqargs  (caadr ika))
        (optargs  (cadadr ika))
        (labels   '()))
    (let ((ccb (make-compiled-code-builder reqargs optargs name #f #f #f)))
      (let lp ((ika (cddr ika))
               (maxstack 0))  ; how do we set this?
        (cond ((null? ika)
               (compiled-code-finish-builder ccb maxstack)
               ccb)

              ;; pseduo insn.  Just label for now.
              ((and (pair? (car ika)) (eq? (caar ika) 'label))
               (let ((lid (cond ((assoc (cadar ika) labels) => cdr)
                                (else (error "no such label" (cadar ika))))))
                 (compiled-code-set-label! ccb lid)
                 (lp (cdr ika) maxstack)))

              ;; VM insn.
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
                      (let ((opcode (~ info' code))
                            (code   (cadr ika)))
                        (cond ((pair? code)
                               (compiled-code-emit2o! ccb opcode arg0 arg1 (ika->vm-code code)))
                              ((is-a? code <compiled-code>)
                               (compiled-code-emit2o! ccb opcode arg0 arg1 code))
                              (else
                               (error "operand has to be a <compiled-code> or ika program, but got " code)))
                        (lp (cddr ika) maxstack)))

                     ((codes)
                      (let ((opcode (~ info' code))
                            (codes  (cadr ika)))
                        (if (not (pair? codes))
                          (error "ika: list of codes required but got " codes))
                        (compiled-code-emit2o! ccb opcode arg0 arg1
                                               (map (lambda (code)
                                                      (if (pair? code)
                                                        (ika->vm-code code)
                                                        code))
                                                    codes))
                        (lp (cddr ika) maxstack)))
                     
                     ((obj)
                      (let ((opcode (~ info' code))
                            (obj    (cadr ika)))
                        (if (and (pair? obj) (eq? 'mkid (car obj)))
                          (compiled-code-emit2o! ccb opcode arg0 arg1 (make-identifier (cadr obj) (find-module 'user) '()))
                          (compiled-code-emit2o! ccb opcode arg0 arg1 obj))
                        (lp (cddr ika) maxstack)))

                     ((addr)
                      (let ((opcode (~ info'code))
                            (addr   (cadr ika)))
                        (cond ((and (pair? addr) (eq? 'label (car addr)))
                               (let ((lid (compiled-code-new-label ccb)))
                                 (push! labels (cons (cadr addr) lid))
                                 (compiled-code-emit2o! ccb opcode arg0 arg1 lid)))
                              ((integer? addr)
                               (compiled-code-emit2o! ccb opcode arg0 arg1 addr))
                              (else
                               (error "ika: addr has to be integer or label, but got " addr)))
                        (lp (cddr ika) maxstack)))

                     ((obj+addr)
                      (let ((opcode  (~ info'code))
                            (obj     (cadr ika))
                            (addr    (caddr ika)))
                        (cond ((and (pair? addr) (eq? 'label (car addr)))
                               (let ((lid (compiled-code-new-label ccb)))
                                 (push! labels (cons (cadr addr) lid))
                                 (compiled-code-emit0o! ccb opcode (list obj lid)))) ; we know no args in this case.
                              ((integer? addr)
                               (compiled-code-emit0o! ccb opcode (list obj addr)))   ; we know no args in this case.
                              (else
                               (error "ika: addr has to be integer or label, but got " addr)))
                        (lp (cdddr ika) maxstack)))

                     (else
                      (let ((operand (cadr ika)))
                        (compiled-code-emit2o! ccb (~ info'code) arg0 arg1 operand)
                        (lp (cddr ika) maxstack)))

                     ))))

              (else
               (error "ika: syntax error")))))))

;;;
;;;
;;;
(define (c sexp)
  (let ((cc (compile sexp (interaction-environment))))
    (newline)
    (ika/pp (append '(%top-level (0 0)) (vm-code->list cc)))
    (newline)
    (vm-dump-code cc)
    ))

#|

(c '(+ 1 2))
(c '(lambda (a b c) (fobar a b c)))

(c '(define (hello)
      (print "hello, world")))

(c '(define (fact n)
      (if (= n 1)
        1
        (* n (fact (- n 1))))))

(c '(define (fo a)
      (if (eq? a 'fobar)
        'yes
        'no)))

|#


;; EOF
