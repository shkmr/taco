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
          integer-fits-insn-arg?
          unsigned-integer-fits-insn-arg?
          compile
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
             integer-fits-insn-arg?
             unsigned-integer-fits-insn-arg?
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

  (define (sp level n) (format #f "~va~4,' d " level "" n))

  (define (pi level n str info)
    (let ((str (string-append (sp level n) str)))
      (display str)
      (if info
        (let ((len (string-length str)))
          (if (< len 40)
            (dotimes ((- 40 len)) (display #\space)))
          (format #t "; ~,,,,40:s" info)))
      (newline)))

  (define (p0 level n opcode info)
    (pi level n (format #f "~s" opcode) info))

  (define (p1 level n opcode operand info)
    (pi level n (format #f "~s ~s" opcode operand) info))

  (define (p2 level n opcode obj addr info)
    (pi level n (format #f "~s ~s ~s" opcode obj addr) info))

  (define (pcode level n opcode name args info)
    (pi level n (format #f "~s (~s ~s" opcode name args) info))

  (define (pcodes level n opcode info)
    (pi level n (format #f "~s (" opcode) info))

  (define (ff cp level n i)
    (cond ((null? cp) #t)

          ;; pseudo insn. label and info
          ((and (pair? (car cp)) (eq? (caar cp) 'label))
           (format #t "~va     ~s~%" level "" (car cp))
           (ff (cdr cp) level n #f))

          ((and (pair? (car cp)) (eq? (caar cp) 'info))
           (ff (cdr cp) level n (cdar cp)))

          ;; VM insn.
          ((pair? (car cp))
           (let ((info (vm-find-insn-info (caar cp))))
             (case (~ info'operand-type)

               ((none)
                (p0 level n (car cp) i)
                (ff (cdr cp) level (+ n 1) #f))

               ((code)
                (let ((opcode  (car cp))
                      (operand (cadr cp)))
                  (cond ((is-a? operand <compiled-code>)
                         (let ((name (compiled-code-name operand))
                               (args (compiled-code-args operand)))
                           (pcode level n opcode name args operand)
                           (ff (vm-code->list operand) (+ level 4) 0 #f)
                           (format #t "~va     )~%" level "")))
                        ((and (pair? operand) (symbol? (car operand)))
                         (let ((name (car operand))
                               (args (cadr operand)))
                           (pcode level n opcode name args i)
                           (ff (cddr operand) (+ level 4) 0 #f)
                           (format #t "~va     )~%" level "")))
                        (else
                         (error "ika: operand has to be <compiled-code> or ika program, but got "
                                operand)))
                  (ff (cddr cp) level (+ n 2) #f)))

               ((codes)
                (let ((opcode  (car cp))
                      (operand (cadr cp)))
                  (if (not (pair? operand))
                    (error "ika: operand has to be list of codes, but got " operand))
                  (cond ((and (pair? (car operand)) (symbol? (caar operand)))
                         (pcodes level n opcode i)
                         (for-each (lambda (prog)
                                     (format #t "~va     (~s ~s~%"
                                             level "" (car prog) (cadr prog))
                                     (ff (cddr prog) (+ level 4) 0 #f)
                                     (format #t "~va)~%" level ""))
                                   operand)
                         (format #t "~va     )~%" level ""))

                        (else
                         (pcodes level n opcode i)
                         (for-each (lambda (cc)
                                     (format #t "~va       ~s~%" level "" cc)
                                     (if (is-a? cc <compiled-code>)
                                       (ff (vm-code->list cc) (+ level 4) 0 #f)))
                                   operand)
                         (format #t "~va     )~%" level "")))
                  (ff (cddr cp) level (+ n 2) #f)))

               ((obj+addr)
                (let ((opcode (car cp))
                      (obj    (cadr cp))
                      (addr   (caddr cp)))
                  (p2 level n opcode obj addr i)
                  (ff (cdddr cp) level (+ n 3) #f)))

               (else
                (p1 level  n (car cp) (cadr cp) i)
                (ff (cddr cp) level (+ n 2) #f)))))
          (else
            (error "ika: syntax error: " cp))))
  (format #t "(~s ~s~%" (car prog) (cadr prog))
  (ff (cddr prog) 4 0 #f))

;;;
;;;
;;;
(define (ika->vm-code ika)
  (let ((name     (car ika))
        (reqargs  (caadr ika))
        (optargs  (cadadr ika)))

    (let ((ccb (make-compiled-code-builder reqargs optargs name #f #f #f))
          (labels '()))

      (define (get-label-id label)
        (cond ((assoc label labels) => cdr)
              (else
               (let ((lid (compiled-code-new-label ccb)))
                 (push! labels (cons label lid))
                 lid))))

      (let lp ((ika (cddr ika))
               (i   #f)
               (maxstack 0))  ; how do we set this?
        (cond ((null? ika)
               (compiled-code-finish-builder ccb maxstack)
               ccb)

              ;; pseduo insn.  label and info.
              ((and (pair? (car ika)) (eq? (caar ika) 'label))
               (let ((lid (get-label-id (cadar ika))))
                 (compiled-code-set-label! ccb lid)
                 (lp (cdr ika) #f maxstack)))

              ((and (pair? (car ika)) (eq? (caar ika) 'info))
               (lp (cdr ika) (cdar ika) maxstack))

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
                      (compiled-code-emit2i! ccb (~ info'code) arg0 arg1 i)
                      (lp (cdr ika) #f  maxstack))

                     ((code)
                      (let ((opcode (~ info' code))
                            (code   (cadr ika)))
                        (cond ((pair? code)
                               (compiled-code-emit2oi! ccb opcode arg0 arg1 (ika->vm-code code) i))
                              ((is-a? code <compiled-code>)
                               (compiled-code-emit2oi! ccb opcode arg0 arg1 code i))
                              (else
                               (error "ika: operand has to be a <compiled-code> or ika program, but got " code)))
                        (lp (cddr ika) #f maxstack)))

                     ((codes)
                      (let ((opcode (~ info' code))
                            (codes  (cadr ika)))
                        (if (not (pair? codes))
                          (error "ika: list of codes required but got " codes))
                        (compiled-code-emit2oi! ccb opcode arg0 arg1
                                               (map (lambda (code)
                                                      (if (pair? code)
                                                        (ika->vm-code code)
                                                        code))
                                                    codes)
                                               i)
                        (lp (cddr ika) #f maxstack)))

                     ((obj)
                      (let ((opcode (~ info' code))
                            (obj    (cadr ika)))
                        (if (and (pair? obj) (eq? 'mkid (car obj)))
                          (let ((id (make-identifier (cadr obj) (find-module 'user) '())))
                            (compiled-code-emit2oi! ccb opcode arg0 arg1 id i))
                          (compiled-code-emit2oi! ccb opcode arg0 arg1 obj i))
                        (lp (cddr ika) #f maxstack)))

                     ((addr)
                      (let ((opcode (~ info'code))
                            (addr   (cadr ika)))
                        (cond ((and (pair? addr) (eq? 'label (car addr)))
                               (let ((lid (get-label-id (cadr addr))))
                                 (compiled-code-emit2oi! ccb opcode arg0 arg1 lid i)))
                              ((integer? addr)
                               (compiled-code-emit2oi! ccb opcode arg0 arg1 addr i))
                              (else
                               (error "ika: addr has to be integer or label, but got " addr)))
                        (lp (cddr ika) #f maxstack)))

                     ((obj+addr)
                      (let ((opcode  (~ info'code))
                            (obj     (cadr ika))
                            (addr    (caddr ika)))
                        (cond ((and (pair? addr) (eq? 'label (car addr)))
                               (let ((lid (get-label-id (cadr addr))))
                                 ;; we know no args in this case.
                                 (compiled-code-emit0oi! ccb opcode (list obj lid) i)))
                              ((integer? addr)
                               ;; we know no args in this case.
                               (compiled-code-emit0oi! ccb opcode (list obj addr) i))
                              (else
                               (error "ika: addr has to be integer or label, but got " addr)))
                        (lp (cdddr ika) #f maxstack)))

                     (else
                      (let ((operand (cadr ika)))
                        (compiled-code-emit2oi! ccb (~ info'code) arg0 arg1 operand i)
                        (lp (cddr ika) #f maxstack)))

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

(provide "ika")
;; EOF
