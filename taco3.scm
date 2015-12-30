;;;
;;;  TACO3 is TACO2 retargeted to current Gauche VM (from 0.8.4 and on),
;;;
;;;  TACO2 is TACO1 + Simple optimization.
;;;
;;;  TACO1 is something similar to hoc6 describied in
;;;  ``The UNIX Programming Environment'' by Kernighan and Pike,
;;;  implemented in Gauche and the target VM is MGVM.
;;;
;;;  MGVM (Mock-GVM) is a subset of the first generation Gauche VM
;;;  (corresponding to Gauche version upto 0.8.3), implemented in Gauche
;;;  (for me to study stack frame of Gauche VM at the time).
;;;
;;;  Useful documents:
;;;
;;;   http://practical-scheme.net/docs/stack-j.html
;;;   http://practical-scheme.net/gauche/memo-j.html
;;;   http://practical-scheme.net/gauche/memo-stack-j.html
;;;   http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3AVMの最適化
;;;
;;;  Transition from 1st gen to 2nd gen GVM:
;;;
;;;   http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3AVMの最適化%3AFor%200.8.4
;;;
;;;  Restructuring stack frame (1st gen GVM):
;;;
;;;   http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3AVMの最適化%3AFor%200.6.3
;;;
;;;  The source of hoc can be found in
;;;
;;;   http://www.cs.princeton.edu/~bwk/btl.mirror/new/ .
;;;
;;;
(define-module taco3
  (use srfi-1)
  (use lalr)
  (use tlex)
  (use ika)
  (use vmhack)
  (export taco3-parser taco3 taco3-load taco3-eval-string))
(select-module taco3)

(define taco3-parser
  (lalr-parser
   ;; (expect: 0)
   ;; --- token definitions
   (ID IDENTIFIER CONSTANT STRING
       VAR BLTIN UNDEF WHILE IF ELSE RETURN FUNC PROC ARG PRINT READ
       ;; FUNCTION PROCEDURE
       LPAREN RPAREN LCBRA RCBRA LSBRA RSBRA COMMA OR NEWLINE
       (right: =)
       (left: OROR)
       (left: &&)
       (left: < <= > >= == !=)
       (left: + -)
       (left: * /)
       (nonassoc: uminus !)
       (right: ^))
   ;;
   ;; --- rules
   (top    ()
           (top NEWLINE)
           (top defn NEWLINE)    : (taco-compile-and-run $2)
           (top stmt NEWLINE)    : (taco-compile-and-run $2)
           ;;(top asgn NEWLINE)
           ;;(top expr NEWLINE)
           (error    NEWLINE)
           )

   (asgn   (IDENTIFIER = expr)   : (list 'VARSET $1 $3)
           (ARG = expr)          : (list 'ARGSET $1 $3)
           )

   (stmt   (expr)                   : $1
           (RETURN)                 : (list 'RETURN)
           (RETURN expr)            : (list 'RETURN $2)
           ;;(PROCEDURE LPAREN arglist RPAREN) : (list 'FUNCALL $1 $3)
           (PRINT prlist)           : (list 'PRINT $2)
           (while cond stmt)        : (list 'WHILE $2 $3)
           (if cond stmt ELSE stmt) : (list 'IFEL $2 $3 $5)
           (if cond stmt)           : (list 'IF $2 $3)
           (LCBRA stmtlist RCBRA)   : (cons 'BEGIN $2)
           )

   (cond   (LPAREN expr RPAREN) : $2 )

   (while  (WHILE)  )
   (if     (IF)     )

   (stmtlist ()                    : '()
             (stmtlist NEWLINE)    : $1
             (stmtlist stmt)       : (append $1 (list $2))
             )

   (expr   (CONSTANT)              : (list 'CONSTANT (cadr $1))
           (IDENTIFIER)            : (list 'VARREF $1)
           (ARG)                   : (list 'ARGREF $1)
           (asgn)                  : $1
           (IDENTIFIER LPAREN arglist RPAREN) : (list 'FUNCALL $1 $3)
           (READ LPAREN IDENTIFIER RPAREN)    : (list 'READ $3)
           (LPAREN expr RPAREN)    : $2
           (expr + expr)           : (list 'ADD $1 $3)
           (expr - expr)           : (list 'SUB $1 $3)
           (expr * expr)           : (list 'MUL $1 $3)
           (expr / expr)           : (list 'DIV $1 $3)
           (expr ^ expr)           : (list 'POW $1 $3)
           (- expr (prec: uminus)) : (list 'NEGATE $2)
           (expr > expr)           : (list 'NUMCMP '((NUMGT2)) $1 $3)
           (expr >= expr)          : (list 'NUMCMP '((NUMGE2)) $1 $3)
           (expr <  expr)          : (list 'NUMCMP '((NUMLT2)) $1 $3)
           (expr <= expr)          : (list 'NUMCMP '((NUMLE2)) $1 $3)
           (expr == expr)          : (list 'NUMCMP '((NUMEQ2)) $1 $3)
           (expr != expr)          : (list 'NUMCMP '((NUMEQ2) (NOT)) $1 $3)
           (expr && expr)          : (list 'AND $1 $3)
           (expr OROR expr)        : (list 'OR  $1 $3)
           (! expr)                : (list 'NOT $2)
           )

   (prlist (expr)                  : (list $1)
           (STRING)                : (list (list 'CONSTANT $1))
           (prlist COMMA expr)     : (append $1 (list $3))
           (prlist COMMA STRING)   : (append $1 (list (list 'CONSTANT $3)))
           )

   (defn   (FUNC procname LPAREN CONSTANT RPAREN stmt)  : (if (eq? (car $4) 'int)
                                                            (list 'DEF 'FUNC $2 (cadr $4) $6)
                                                            (errorp "in function definition"))
           (PROC procname LPAREN CONSTANT RPAREN stmt)  : (if (eq? (car $4) 'int)
                                                            (list 'DEF 'PROC $2 (cadr $4) $6)
                                                            (errorp "in procedure definition"))
           )

   (procname (IDENTIFIER) : $1
             )

   (arglist  ()                    : '()
             (expr)                : (list $1)
             (arglist COMMA expr)  : (append $1 (list $3))
             )
   ))

;;;
;;;  COMPILER
;;;
(define *ika* #f)      ; last complied ika code. (used in test-taco3.scm)
(define *verbose* #f)  ; compiler flag

(define (taco-compile-and-run tree)

  (define (mess . x) (if *verbose* (apply print x)))

  (let ((result #f) (vmcode #f))
    (mess "tree: ")
    (if *verbose* (write tree))
    (reset-label)
    (set! *ika* `(%top-level (0 0) ,@(tacomp tree 0 #f) (RET)))
    (mess "=== ika program ===")
    (if *verbose* (ika/pp *ika*))
    (set! vmcode (ika->vm-code *ika*))
    (if *verbose* (vm-dump-code vmcode))
    (set! result (vm-code-execute! vmcode (interaction-environment)))
    (mess "=> " result)
    result))

;;
(define (const? e)    (eq? 'CONST (caar e)))
(define (const-val e) (cadr e))

(define *labelno* 0)
(define (new-label)   (inc! *labelno*) *labelno*)
(define (reset-label) (set! *labelno* 0))

;;
(define (tacomp tree level indef)
  ;;
  ;; indef : #f  -> toplevel
  ;;        : int -> # of args of the func being defn'ed
  ;;
  ;; level  : # of env levels (0 = toplevel)
  ;;
  (define (op tree)  (car tree))
  (define (op-args tree)  (cdr tree))
  (define (op-arg1 tree)  (list-ref tree 1))
  (define (op-arg2 tree)  (list-ref tree 2))
  (define (op-arg3 tree)  (list-ref tree 3))
  (define (op-arg4 tree)  (list-ref tree 4))

  (cond
   ((null? tree)       '())
   ((not (pair? tree)) (list tree))
   (else
    (case (op tree)

      ((CONSTANT) (list '(CONST) (op-arg1 tree)))

      ((ADD)
       (let ((d1 (tacomp (op-arg1 tree) level indef))
             (d2 (tacomp (op-arg2 tree) level indef)))
         (cond ((and (const? d1)
                     (const? d2))
                `((CONST) ,(+ (const-val d1) (const-val d2))))
               ((and (const? d2)
                     (integer-fits-insn-arg? (const-val d2)))
                `(,@d1
                  (NUMADDI ,(const-val d2))))
               ((and (const? d1)
                     (integer-fits-insn-arg? (const-val d1)))
                `(,@d2
                  (NUMADDI ,(const-val d1))))
               (else
                `(,@d1
                  (PUSH)
                  ,@d2
                  (NUMADD2))))))

      ((SUB)
       (let ((d1 (tacomp (op-arg1 tree) level indef))
             (d2 (tacomp (op-arg2 tree) level indef)))
         (cond ((and (const? d1)
                     (const? d2))
                (list '(CONST) (- (const-val d1) (const-val d2))))
               ((and (const? d1)
                     (integer-fits-insn-arg? (const-val d1)))
                `(,@d2
                  (NUMSUBI ,(const-val d1))))
               ((and (const? d2)
                     (integer-fits-insn-arg? (const-val d2)))
                `(,@d1
                  (NUMADDI ,(- (const-val d2)))))
               (else
                `(,@d1
                  (PUSH)
                  ,@d2
                  (NUMSUB2))))))

      ((MUL)
       (let ((d1 (tacomp (op-arg1 tree) level indef))
             (d2 (tacomp (op-arg2 tree) level indef)))
         (if (and (const? d1) (const? d2))
           (list '(CONST) (* (const-val d1) (const-val d2)))
           `(,@d1
             (PUSH)
             ,@d2
             (NUMMUL2)))))
      ((DIV)
       (let ((d1 (tacomp (op-arg1 tree) level indef))
             (d2 (tacomp (op-arg2 tree) level indef)))
         (if (and (const? d1) (const? d2))
           (list '(CONST) (/ (const-val d1) (const-val d2)))
           `(,@d1
             (PUSH)
             ,@d2
             (NUMDIV2)))))

      ((POW)
       (let ((d1 (tacomp (op-arg1 tree) level indef))
             (d2 (tacomp (op-arg2 tree) level indef))
             (L1 (new-label)))
         (if (and (const? d1) (const? d2))
           (list '(CONST) (expt (const-val d1) (const-val d2)))
           `((PRE-CALL 2) (label ,L1)
             ,@d1
             (PUSH)
             ,@d2
             (PUSH)
             (GREF) (mkid expt)    ; or GREF-CALL?
             (CALL 2)
             (label ,L1)))))

      ((NEGATE)
       (let ((d1 (tacomp (op-arg1 tree) level indef)))
         (if (const? d1)
           (list '(CONST) (- (const-val d1)))
           `(,@d1
             (NEGATE)))))

      ((NUMCMP)
       (let ((insn (op-arg1 tree))
             (d1   (tacomp (op-arg2 tree) level indef))
             (d2   (tacomp (op-arg3 tree) level indef)))
         `(,@d1
           (PUSH)
           ,@d2
           ,@insn)))

      ((NOT)
       (let ((d1 (tacomp (op-arg1 tree) level indef)))
         `(,@d1
           (NOT))))

      ((VARREF)
       (let ((s (op-arg1 tree)))
         `((GREF) (mkid ,s))))

      ((VARSET)
       (let ((s (op-arg1 tree))
             (v (op-arg2 tree)))
         (if (= level 0)
           `(,@(tacomp v level indef)
             (DEFINE 0) (mkid ,s))
           `(,@(tacomp v level indef)
             (GSET) (mkid ,s)))))

      ((ARGREF)
       (if (= level 0)
         (error "$n in top-level")
         (let ((s (op-arg1 tree)))
           `((LREF ,(- level 1) ,(- indef s))
             ))))

      ((ARGSET)
       (if (= level 0)
           (error "$n in top-level")
           (let ((s (op-arg1 tree))
                 (v (op-arg2 tree)))
             `(,@(tacomp v level indef)
               (LSET ,(- level 1) ,(- indef s))
               ))))
      ((FUNCALL)
       (let* ((f    (op-arg1 tree))
              (args (map (lambda (x)
                           (tacomp x level indef))
                         (op-arg2 tree)))
              (n    (length args))
              (prep (append-map (lambda (x)
                                  (append x (list '(PUSH))))
                                args))
              (L1   (new-label)))
         `((PRE-CALL ,n) (label ,L1)
           ,@prep
           (GREF) (mkid ,f)
           (CALL ,n)
           (label ,L1))))

      ((IF)
       (let ((c  (tacomp (op-arg1 tree) level indef))
             (s  (tacomp (op-arg2 tree) level indef))
             (L1 (new-label)))
         `(,@c
           (BF) (label ,L1)
           ,@s
           (label ,L1))))

      ((IFEL)
       (let ((c  (tacomp (op-arg1 tree) level indef))
             (s  (tacomp (op-arg2 tree) level indef))
             (e  (tacomp (op-arg3 tree) level indef))
             (L1 (new-label))
             (L2 (new-label)))
         `(,@c
           (BF)   (label ,L1)
           ,@s
           (JUMP) (label ,L2)
           (label ,L1)
           ,@e
           (label ,L2))))

      ((WHILE)
       (let ((c  (tacomp (op-arg1 tree) level indef))
             (s  (tacomp (op-arg2 tree) level indef))
             (L1 (new-label))
             (L2 (new-label)))
         `((label ,L1)
           ,@c
           (BF) (label ,L2)
           ,@s
           (JUMP) (label ,L1)
           (label ,L2))))

      ((RETURN)
       (if indef
         (let ((r (tacomp (op-arg1 tree) level indef)))
           `(,@r
             (RET)))
         (error "RETURN outside defn")))

      ((PRINT)
       (let* ((args (map (lambda (x) (tacomp x level indef))
                         (op-arg1 tree)))
              (lbls (map (lambda (x) (new-label)) args)))
         (append-map (lambda (item label)
                       `((PRE-CALL 1) (label ,label)
                         ,@item
                         (PUSH)
                         (GREF) (mkid display)
                         (CALL 1)
                         (label ,label)))
                     args lbls)))

      ((BEGIN)
       (append-map (lambda (x) (tacomp x level indef))
                   (op-args tree)))

      ((DEF)
       (let ((f (op-arg2 tree))     ; name
             (n (op-arg3 tree))     ; number of args
             (b (op-arg4 tree)))    ; body
         (if (not indef)
           (let* ((body    (tacomp b 1 n))
                  (mlvars  (find-mutated-lvar body n '())))
             `((CLOSURE) (,f (,n 0)
                           ,@(map (lambda (i) (list 'BOX (+ i 1))) mlvars)
                           ,@(insert-unbox body mlvars)
                           (RET))
               (DEFINE 0) (mkid ,f)))
           (error "func or proc has to be at the top level"))))

      (else
       (error "not implemented yet"))))))

(define (find-mutated-lvar body n mlvars)
  (cond ((null? body) mlvars)
        ((and (pair? (car body)) (eq? 'LSET (caar body)))
         (let ((depth  (cadar  body))
               (offset (caddar body)))
           (if (or (not (= depth 0))
                   (>= offset n))
             (error "something went wrong" `(LSET ,depth ,offset)))
           (find-mutated-lvar (cdr body) n (cons offset mlvars))))
        (else
         (find-mutated-lvar (cdr body) n mlvars))))

;; assume all lvars get mutated.
(define (find-mutated-lvar-0 body n mlvars) (iota n))

(define (insert-unbox body mlvars)
  (append-map (lambda (e)
                (if (and (pair? e)
                         (eq? (car e) 'LREF)
                         (memq (caddr e) mlvars))
                  (list e '(UNBOX))
                  (list e)))
              body))
;;;
;;;  API
;;;
(define (taco3)                 (taco3-parser tlex error))
(define (taco3-load file)       (with-input-from-file file taco3))
(define (taco3-eval-string str) (with-input-from-string str taco3))

(provide "taco3")
;;; EOF
