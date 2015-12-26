(define-module mgvm (export mgvm/init mgvm/run mgvm/pp mgvm/dump))
(select-module mgvm)
;;;
;;;  Mock-Gauche-Virtual-Machine.
;;;

(define *stack-size* 300000)
(define undef (if #f #t))

;;;
;;; Machine registers
;;;

(define env  '())
(define cont '())
(define argp 0)
(define sp  0)
(define pc '())
(define r0 undef)  ; accumulator (val0)

;; pc
(define (inc-pc!) (set! pc (cdr pc)))
(define (bra-pc!) (set! pc (cadr pc)))

;; sp
(define stack (make-vector *stack-size*))

(define (stack-ref n)
  (vector-ref stack (+ sp n)))

(define (stack-set! n v)
  (vector-set! stack (+ sp n) v))

(define (purge-stack)

  (define (copy-only-in-use new old)
    (do ((i 0 (+ i 1))
         (sp sp))
        ((= *stack-size* sp))
      (vector-set! new i (vector-ref old i)) ;; TODO
      ))

  (let* ((newstack (make-vector *stack-size*))
         (newsp (copy-only-in-use newstack stack)))
    (if newsp
        (begin
          (set! sp newsp)
          (set! stack newstack))
        (error "MGVM: stack overflow"))))

(define (check-stack n)
  (let ((n (+ sp n)))
    (cond ((< n 0)
           (error "MGVM: stack underflow"))
          ((>= n *stack-size*)
           (purge-stack)
           (check-stack n)))))

(define max-stack 0)

(define (push val)
  (check-stack 1)
  (vector-set! stack sp val)
  (inc! sp)
  (set! max-stack (max sp max-stack)))

(define (pop)
  (dec! sp)
  (vector-ref stack sp))

;;;
;;;  Frame,
;;;  http://www.shiro.dreamhost.com/scheme/gauche/memo-stack-j.html
;;;  http://www.shiro.dreamhost.com/scheme/wiliki/
;;;                    wiliki.cgi?Gauche%3aVM%a4%ce%ba%c7%c5%ac%b2%bd
;;;

;; cont
(define cont-frame-size 6)

(define (cont-prev cont) (vector-ref stack cont))
(define (cont-env  cont) (vector-ref stack (+ cont 1)))
(define (cont-argp cont) (vector-ref stack (+ cont 2)))
(define (cont-size cont) (vector-ref stack (+ cont 3)))
(define (cont-pc   cont) (vector-ref stack (+ cont 4)))
(define (cont-info cont) (vector-ref stack (+ cont 5)))

(define (cont-prev-set! cont p) (vector-set! stack cont p))
(define (cont-env-set!  cont p) (vector-set! stack (+ cont 1) p))
(define (cont-argp-set! cont p) (vector-set! stack (+ cont 2) p))
(define (cont-size-set! cont p) (vector-set! stack (+ cont 3) p))
(define (cont-pc-set!   cont p) (vector-set! stack (+ cont 4) p))
(define (cont-info-set! cont p) (vector-set! stack (+ cont 5) p))

(define (push-cont old_pc next_pc)
  (stack-set! 0 cont)
  (stack-set! 1 env)
  (stack-set! 2 argp)
  (stack-set! 3 (- sp argp))   ; size
  (stack-set! 4 next_pc)       ; pc
  (stack-set! 5 old_pc)        ; info
  (set! cont sp)
  (set! sp (+ sp cont-frame-size))
  (set! argp sp))

(define (pop-cont)
  (set! sp   (+ (cont-argp cont) (cont-size cont)))
  (set! env  (cont-env cont))
  (set! argp (cont-argp cont))
  (set! pc   (cont-pc cont))
  (set! cont (cont-prev cont)))

;; env
(define env-header-size 3)

(define (finish-env info up)
  (stack-set! 0 up)
  (stack-set! 1 info)
  (stack-set! 2 (- sp argp))
  (set! env sp)
  (set! sp (+ sp env-header-size))
  (set! argp sp))

(define (env-up env)          (vector-ref stack env))
(define (env-info env)        (vector-ref stack (+ env 1)))
(define (env-size env)        (vector-ref stack (+ env 2)))
(define (env-up-set!   env p) (vector-set! stack env p))
(define (env-info-set! env p) (vector-set! stack (+ env 1) p))
(define (env-size-set! env p) (vector-set! stack (+ env 2) p))

(define (env-data env n) 
  (vector-ref stack (- env n 1)))
(define (env-data-set! env n o) 
  (vector-set! stack (- env n 1) o))

(define (env-fp env) (- env (env-size env)))

(define (push-local-env size info)
  (do ((i 0 (+ i 1)))
      ((>= i size))
    (push undef))
  (finish-env info env))

(define (pop-local-env)
  (set! sp (- sp env-header-size (env-size env)))
  (set! env (env-up env))
  (if (< sp 0) (set! sp 0))
  (set! argp sp))

;;;
;;; Globals
;;;

(define glo-hash (make-hash-table))
(define (glo-set! s v)  (hash-table-put! glo-hash s v))
(define (glo-exists? s) (hash-table-exists? glo-hash s))
(define (glo-lookup s) 
  (if (hash-table-exists? glo-hash s)
      (hash-table-get glo-hash s)
      (if (symbol-bound? s)
          (eval s (interaction-environment))
          (errorf "unknown global ~a~%" s))))

;;;
;;; Funnction object
;;;

(define (function? obj)      (and (pair? obj) (eq? (car obj) 'function)))
(define (function-narg obj)  (cadr obj))
(define (function-info obj)  (caddr obj))
(define (function-env obj)   (cadddr obj))
(define (function-entry obj) (cddddr obj))

(define (make-function narg nopt code info)
  (cons 'function (cons narg (cons info (cons env code)))))

(define (adjust-argument-frame f argc) #t)

;;;
;;; program = ((INSN)
;;;            (INSN ARG)
;;;            IMM
;;;            (IF)
;;;             ((INSN) (INSN) ...)
;;;             (INSN))
;;;

(define (insn? code)
  (and (pair? code)
       (pair? (car code))
       (symbol? (caar code))))

(define (code->insn-name code) (caar code))
(define (code->insn-args code) (cdar code))

(define (code->insn-proc code)
  (let ((name (code->insn-name code)))
    (if (hash-table-exists? insn-hash name)
        (hash-table-get insn-hash name)
        (errorf "vm: error unknown insn: ~a" name))))

;;;
;;; Machine Instructions
;;;

(define insn-hash (make-hash-table))

(define-syntax definsn
  (syntax-rules ()
    ((definsn (name) . body)
     (hash-table-put! insn-hash 'name (lambda () . body)))
    ((definsn (name args) . body)
     (hash-table-put! insn-hash 'name (lambda (args) . body)))
    ((definsn name args . body)
     (hash-table-put! insn-hash 'name (lambda args . body)))
    ))

(definsn (NOP)  (inc-pc!))
(definsn (PUSH) (inc-pc!) (push r0))
(definsn (POP)  (inc-pc!) (set! r0 (pop)))
(definsn (DUP)  (inc-pc!) (push (stack-ref -1)))
(definsn (PUSHI n) (inc-pc!) (push n))
                     
(definsn (NUMADD2)   (inc-pc!) (set! r0 (+ (pop) r0)))
(definsn (NUMADDI n) (inc-pc!) (set! r0 (+ n r0)))
(definsn (NUMSUB2)   (inc-pc!) (set! r0 (- (pop) r0)))
(definsn (NUMSUBI n) (inc-pc!) (set! r0 (- n r0)))
(definsn (NUMEQ2)    (inc-pc!) (set! r0 (= r0 (pop))))

(definsn LREF (n m)  
  (inc-pc!)
  (do ((n (- n 1) (- n 1))
       (e env (env-up e)))
      ((< n 0)
       (set! r0 (env-data e m)))))

(definsn (LREF0)  (inc-pc!) (set! r0 (env-data env 0)))
(definsn (LREF1)  (inc-pc!) (set! r0 (env-data env 1)))
(definsn (LREF2)  (inc-pc!) (set! r0 (env-data env 2)))
(definsn (LREF3)  (inc-pc!) (set! r0 (env-data env 3)))
(definsn (LREF4)  (inc-pc!) (set! r0 (env-data env 4)))
(definsn (LREF10) (inc-pc!) (set! r0 (env-data (env-up env) 0)))
(definsn (LREF11) (inc-pc!) (set! r0 (env-data (env-up env) 1)))
(definsn (LREF12) (inc-pc!) (set! r0 (env-data (env-up env) 2)))
(definsn (LREF13) (inc-pc!) (set! r0 (env-data (env-up env) 3)))
(definsn (LREF14) (inc-pc!) (set! r0 (env-data (env-up env) 4)))

(definsn LREF-PUSH (n m)
  (inc-pc!)
  (do ((n (- n 1) (- n 1))
       (e env (env-up e)))
      ((< dep 0)
       (set! r0 (env-data e m))
       (push r0))))

(definsn (LREF0-PUSH)  (inc-pc!) (set! r0 (env-data env 0)) (push r0))
(definsn (LREF1-PUSH)  (inc-pc!) (set! r0 (env-data env 1)) (push r0))
(definsn (LREF2-PUSH)  (inc-pc!) (set! r0 (env-data env 2)) (push r0))
(definsn (LREF3-PUSH)  (inc-pc!) (set! r0 (env-data env 3)) (push r0))
(definsn (LREF4-PUSH)  (inc-pc!) (set! r0 (env-data env 4)) (push r0))
(definsn (LREF10-PUSH) (inc-pc!) (set! r0 (env-data (env-up env) 0)) (push r0))
(definsn (LREF11-PUSH) (inc-pc!) (set! r0 (env-data (env-up env) 1)) (push r0))
(definsn (LREF12-PUSH) (inc-pc!) (set! r0 (env-data (env-up env) 2)) (push r0))
(definsn (LREF13-PUSH) (inc-pc!) (set! r0 (env-data (env-up env) 3)) (push r0))
(definsn (LREF14-PUSH) (inc-pc!) (set! r0 (env-data (env-up env) 4)) (push r0))

(definsn LSET (n m)
  (inc-pc!) 
  (do ((n (- n 1) (- n 1))
       (e env (env-up e)))
      ((< dep 0)
       (env-data-set! e m r0))))

(definsn (LSET0)  (inc-pc!) (env-data-set! env 0 r0))
(definsn (LSET1)  (inc-pc!) (env-data-set! env 1 r0))
(definsn (LSET2)  (inc-pc!) (env-data-set! env 2 r0))
(definsn (LSET3)  (inc-pc!) (env-data-set! env 3 r0))
(definsn (LSET4)  (inc-pc!) (env-data-set! env 4 r0))

(definsn (GREF)
  (inc-pc!)
  (if (symbol? (car pc))
      (let ((obj (glo-lookup (car pc))))
        (if (procedure? obj) (set-car! pc obj)) ;
        (set! r0 obj))
      (set! r0 (car pc)))
  (inc-pc!))

(definsn (GSET)
  (inc-pc!)
  (if (and (symbol? (car pc))
           (glo-exists? (car pc)))
      (glo-set! (car pc) r0)
      (errorf "GSET: unknown global ~a~%" (car pc)))
  (inc-pc!))

(definsn (DEFINE)
  (inc-pc!)
  (if (symbol? (car pc))
      (glo-set! (car pc) r0)
      (errorf "DEFINE: ~a must be symbol~%" (car pc)))
  (inc-pc!))

(definsn (QUOTE) ; to assign a pair to r0
  (inc-pc!)
  (set! r0 pc)
  (inc-pc!))

(definsn LAMBDA (n m)
  (set! r0 (make-function n m (cadr pc) pc))
  (inc-pc!)
  (inc-pc!))

(definsn (LET n)  
  (check-stack (+ n env-header-size cont-frame-size))
  (if (not (null? (cddr pc)))
      (push-cont pc (cddr pc)))
  (push-local-env n pc)
  (bra-pc!))

(definsn (PRE-CALL n)
  (let ((prep (cadr pc))
        (next (cddr pc)))
    (check-stack (+ cont-frame-size env-header-size n 1))
    (if (not (null? next))
        (push-cont pc next))
    (set! pc prep)))

(definsn (PRE-TAIL n)
  (inc-pc!)
  (check-stack (+ n env-header-size 1)))

(define (insn-call-func n)
  (let ((argc (- sp argp)))
    (cond ((procedure? r0) 
           (adjust-argument-frame r0 argc)
           (set! pc '())
           (set! r0 (apply r0 (let lp ((narg n) (args '()))
                                (if (<= narg 0)
                                    args
                                    (lp (- narg 1) (cons (pop) args))))))
           )
          ((function? r0)
           (adjust-argument-frame r0 argc)
           (if (= argc 0)
               (begin
                 (set! env (function-env r0))
                 (set! argp sp))
               (finish-env (function-info r0) (function-env r0)))
           (if (= n (function-narg r0))
               (set! pc (function-entry r0))
               (errorf "SVM: wrong number of arguments, ~a required, got ~a"
                       (function-narg r0) n)))
          (else
           (errorf "~a is not a function" r0)))))

(definsn (CALL n)      (insn-call-func n))

(definsn (TAIL-CALL n) (insn-call-func n))
#|
  (let ((argc (- sp argp))
        (to   (if (null? cont) 0 (+ cont cont-frame-size))))
    (do ((i 0 (+ i 1)))
        ((>=  i argc))
      (vector-set! stack (+ to i) (vector-ref stack (+ argp i))))
    (set! argp to)
    (set! sp (+ to argc))
    (set! env '())
    (insn-call-func n)))
|#

(definsn (RET)  (set! pc '()))

(definsn (IF) 
  (if r0 
      (bra-pc!) 
      (begin (inc-pc!) 
             (inc-pc!))))

;;;
;;; MGVM main loop
;;;
(define steps 0)

(define (run v)
  (inc! steps)
  (if (null? pc)
      (if (null? cont)
          r0
          (begin
            (pop-cont)
            (run v)))
      (begin
        (if v (show))
        (if (insn? pc)
            (apply (code->insn-proc pc)  (code->insn-args pc))
            (begin 
              (set! r0 (car pc))
              (inc-pc!)))
        (run v))))

;;;
;;; so called SUBRs
;;;
(define (mgvm-apply proc args)
  (let ((n (length args)))
    (check-stack n)
    (for-each push args)
    (set! pc `((TAIL-CALL ,n)))
    proc)
  )

(glo-set! 'apply mgvm-apply)

(define (mgvm-make-cont-proc cc)
  (lambda (x)
    (set! cont cc)
    (set! pc '())
    x))

(define (mgvm-call/cc proc)
  (if (and (function? proc)
           (= (function-narg proc) 1))
      (mgvm-apply proc (list (mgvm-make-cont-proc cont)))))

(glo-set! 'call/cc mgvm-call/cc)

;;;
;;;  INFO PUNCHER
;;;
(define (show) 
  (newline)
  (display "")
  (format #t "~%INSN= ~18,,,,18s~%" (car pc))
  (format #t "  pc= ~60,,,,60:s  ~%" pc)
  (format #t "  r0= ~60,,,,60:s  ~%" r0)
  (format #t "  sp= ~5a, argp= ~a, env= ~a, cont ~a~%"
          sp argp env cont)
  (format #t "STACK~%")
  (do ((i (+ sp 5) (- i 1))
       (c 0   (+ c 1)))
      ((or (< i 0) (> c 20)) #t)
    (let ((pts (string-join (let lp ((r '()) 
                                     (s  (list "sp" "argp" "env" "cont"))
                                     (t  (list  sp   argp   env   cont)))
                              (if (null? s)
                                  r
                                  (if (eq? i (car t))
                                      (lp (cons (car s) r) (cdr s) (cdr t))
                                      (lp r (cdr s) (cdr t)))))
                            ",")))
      (format #t "~8a[~4,'0d] = ~60,,,,60:s~%" pts i (vector-ref stack i))))
  )


(define (dump)
  (format #t "  pc= ~60,,,,60:a~%" pc)
  (format #t "  r0= ~60,,,,60:s~%" r0)
  (format #t "  sp= ~5a, argp= ~a, env= ~a, cont ~a~%"
                sp       argp      env      cont)
  (format #t "  steps= ~a, max-stack= ~a~%" steps max-stack)
)

;;;
;;;  EXTERNAL INTERFACES
;;;
(define (mgvm/init . opt)
  (let ((ss (get-keyword :stack-size opt *stack-size*)))
    (if (not (eq? ss *stack-size*))
        (begin
          (set! *stack-size* ss)
          (set! stack (make-vector ss)))
        (vector-fill! stack undef)))
  (if (get-keyword :program opt #f)
      (set! pc (get-keyword :program opt))
      (set! pc '()))
  (set! env '())
  (set! argp 0)
  (set! sp  0) 
  (set! r0 undef)
  (set! max-stack 0)
  (set! steps 0))

(define (mgvm/run . opt)
  (if (get-keyword :program opt #f)
      (set! pc (get-keyword :program opt)))
  (run (get-keyword :verbose opt #f))
  r0)

(define mgvm/dump dump)

(define (mgvm/pp prog)
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

(provide "mgvm")
