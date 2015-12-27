;;;
;;; TACO2 -- TACO1 + Simple optimization
;;;
;;; Simple optimiazation : constant expression
;;;                        + NUMADDI, NUMSUBI
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
   (top
    ()
    (top NEWLINE)
    (top defn NEWLINE)  : (taco-compile-and-run $2)

    (top stmt NEWLINE)  : (taco-compile-and-run $2)

    ;;(top asgn NEWLINE)
    ;;(top expr NEWLINE)

    (error    NEWLINE)
    )

  (asgn
    (IDENTIFIER = expr) : (list 'VARSET $1 $3)
    (ARG = expr)        : (list 'ARGSET $1 $3)
    )

   (stmt
    (expr)                   : $1
    (RETURN)                 : (list 'RETURN)
    (RETURN expr)            : (list 'RETURN $2)
    ;;(PROCEDURE LPAREN arglist RPAREN) : (list 'FUNCALL $1 $3)
    (PRINT prlist)           : (list 'PRINT $2)
    (while cond stmt)        : (list 'WHILE $2 $3)
    (if cond stmt ELSE stmt) : (list 'IFEL $2 $3 $5)
    (if cond stmt)           : (list 'IF $2 $3)
    (LCBRA stmtlist RCBRA)   : (cons 'BEGIN $2)
    )

   (cond
    (LPAREN expr RPAREN) : $2
    )

   (while  (WHILE)  )

   (if     (IF)     )

   (stmtlist
    ()                   : '()
    (stmtlist NEWLINE) : $1
    (stmtlist stmt)      : (append $1 (list $2))
    )

   (expr
    (CONSTANT)              : (list 'CONSTANT (cadr $1))
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
    (expr > expr)           : (list 'FUNCALL '>   (list $1 $3))
    (expr >= expr)          : (list 'FUNCALL '>=  (list $1 $3))
    (expr <  expr)          : (list 'FUNCALL '<   (list $1 $3))
    (expr <= expr)          : (list 'FUNCALL '<=  (list $1 $3))
    (expr == expr)          : (list 'FUNCALL '=   (list $1 $3))
    (expr != expr)          : (list 'NEQ $1 $3)
    (expr && expr)          : (list 'FUNCALL 'and (list $1 $3))
    (expr OROR expr)        : (list 'FUNCALL 'or  (list $1 $3))
    (! expr)                : (list 'FUNCALL 'not (list $2))
    )

   (prlist
    (expr)                : (list $1)
    (STRING)              : (list $1)
    (prlist COMMA expr)   : (append $1 (list $3))
    (prlist COMMA STRING) : (append $1 (list $3))
    )

   (defn
     (FUNC procname LPAREN CONSTANT RPAREN stmt)
     : (if (eq? (car $4) 'int)
           (list 'DEF 'FUNC $2 (cadr $4) $6)
           (errorp "in function definition"))
     (PROC procname LPAREN CONSTANT RPAREN stmt)
     : (if (eq? (car $4) 'int)
           (list 'DEF 'PROC $2 (cadr $4) $6)
           (errorp "in procedure definition"))
     )

   (procname
    (IDENTIFIER) : $1
    )

   (arglist
    ()                    : '()
    (expr)                : (list $1)
    (arglist COMMA expr)  : (append $1 (list $3))
    )
   ))

;;;
;;;  COMPILER
;;;
(define *ika* #f)   ; last complied ika code. (used in test-taco3.scm)

(define (taco-compile-and-run tree)
  (let ((result #f) (vmcode #f))
    (print "tree: " tree)
    (reset-label)
    (set! *ika* `(%top-level (0 0) ,@(tacomp tree 0 #f) (RET)))
    (print "=== ika program ===")
    (ika/pp *ika*)
    (set! vmcode (ika->vm-code *ika*))
    (vm-dump-code vmcode)
    (set! result (vm-code-execute! vmcode (interaction-environment)))
    (print "=> " result)
    result))

;;
(define (const? e)    (eq? 'CONST (caar e)))
(define (const-val e) (cadr e))

(define *labelno* 0)
(define (new-label)   (inc! *labelno*) *labelno*)
(define (reset-label) (set! *labelno* 0))

;;
(define (tacomp tree level indefn)
  ;;
  ;; indefn : #f  -> toplevel
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
       (let ((d1 (tacomp (op-arg1 tree) level indefn))
             (d2 (tacomp (op-arg2 tree) level indefn)))
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
       (let ((d1 (tacomp (op-arg1 tree) level indefn))
             (d2 (tacomp (op-arg2 tree) level indefn)))
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
       (let ((d1 (tacomp (op-arg1 tree) level indefn))
             (d2 (tacomp (op-arg2 tree) level indefn)))
         (if (and (const? d1) (const? d2))
           (list '(CONST) (* (const-val d1) (const-val d2)))
           `(,@d1
             (PUSH)
             ,@d2
             (NUMMUL2)))))
      ((DIV)
       (let ((d1 (tacomp (op-arg1 tree) level indefn))
             (d2 (tacomp (op-arg2 tree) level indefn)))
         (if (and (const? d1) (const? d2))
           (list '(CONST) (/ (const-val d1) (const-val d2)))
           `(,@d1
             (PUSH)
             ,@d2
             (NUMDIV2)))))

      ((POW)
       (let ((d1 (tacomp (op-arg1 tree) level indefn))
             (d2 (tacomp (op-arg2 tree) level indefn))
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
       (let ((d1 (tacomp (op-arg1 tree) level indefn)))
         (if (const? d1)
           (list '(CONST) (- (const-val d1)))
           `(,@d1
             (NEGATE)))))

      ((NEQ)
       (let ((e1 (tacomp (op-arg1 tree) level indefn))
             (e2 (tacomp (op-arg2 tree) level indefn)))
         `((PRE-CALL 1) ((PRE-CALL 2) (,@e1
                                       (PUSH)
                                       ,@e2
                                       (PUSH)
                                       (GREF) =
                                       (CALL 2))
                         (PUSH)
                         (GREF) not
                         (CALL 1)))))

      ((VARREF)
       (let ((s (op-arg1 tree)))
         `((GREF) (mkid ,s))))

      ((VARSET)
       (let ((s (op-arg1 tree))
             (v (op-arg2 tree)))
         (if (= level 0)
             `(,@(tacomp v level indefn)
               (DEFINE 0) (mkid ,s))
             `(,@(tacomp v level indefn)
               (GSET) (mkdid ,s)))))

      ((ARGREF)
       (if (= level 0)
           (error "$n in top-level")
           (let ((s (- (op-arg1 tree) 1)))
             `((LREF ,(- level 1) ,(- indefn s 1))
               )
             )))

      ((ARGSET)
       (if (= level 0)
           (error "$n in top-level")
           (let ((s (op-arg1 tree))
                 (v (op-arg2 tree)))
             `(,@(tacomp v level indefn)
               (LSET ,(- level 1) ,s))
             )))

      ((FUNCALL)
       (let* ((f    (op-arg1 tree))
              (args (map (lambda (x) (tacomp x level indefn))
                         (op-arg2 tree)))
              (n    (length args))
              (prep (append-map (lambda (x)
                                  (append x (list '(PUSH))))
                                args)))
         `((PRE-CALL ,n) (,@prep
                          (GREF) ,f
                          (CALL ,n)))))

      ((IF)
       (let ((c (tacomp (op-arg1 tree) level indefn))
             (s (tacomp (op-arg2 tree) level indefn))
             (l (gensym)))
         `(,@c
           (IF) ,(append s (list (list 'goto l)))
           (label ,l))))

      ((IFEL)
       (let ((c (tacomp (op-arg1 tree) level indefn))
             (s (tacomp (op-arg2 tree) level indefn))
             (e (tacomp (op-arg3 tree) level indefn))
             (l (gensym)))
         `(,@c
           (IF) ,(append s (list (list 'goto l)))
           ,@e
           (label ,l))))

      ((WHILE)
       ;; note: calling closure with 0 argument does not push
       ;;       enviroment. we only have to one level up, not two.
       (let ((c (tacomp (op-arg1 tree) (+ level 1) indefn))
             (s (tacomp (op-arg2 tree) (+ level 1) indefn))
             (l (gensym)))
         `((LET 1) ((LAMBDA 0 0) (,@c
                                  (IF) ,(append s '((PRE-TAIL 0)
                                                    (LREF0)
                                                    (TAIL-CALL 0))))
                    (LSET0)
                    (PRE-CALL 0) ((LREF0)
                                  (CALL 0))))))

      ((RETURN)
       (if indefn
           (let ((r (tacomp (op-arg1 tree) level indefn)))
             `((PRE-TAIL 1)
               ,@r
               (PUSH)
               (LREF ,(- level 2) 0)
               (TAIL-CALL 1)))
           (error "RETURN outside defn")))

      ((PRINT)
       (let* ((args (map (lambda (x) (tacomp x level indefn))
                         (op-arg1 tree))))
         (append-map (lambda (item)
                       `((PRE-CALL 1) (,@item
                                       (PUSH)
                                       (GREF) ,taco-print
                                       (CALL 1))))
                     args)))

      ((BEGIN)
       (append-map (lambda (x) (tacomp x level indefn))
                   (op-args tree)))

      ((DEF)
       (let ((f (op-arg2 tree))
             (n (op-arg3 tree))
             (b (op-arg4 tree)))
         `((LAMBDA ,n 0) ((PRE-CALL 1) ((LAMBDA 1 0) ,(tacomp b 2 n)
                                        (PUSH)
                                        (GREF) call/cc
                                        (CALL 1)))
           (DEFINE) ,f)))

      (else
       (error "not implemented yet"))))))

;;;
;;;  RUNTIME LIBRARIES
;;;
(define (taco-print o) (display o))

(define (taco-print-o o)
  (if (number? o)
      (begin (display o)
             (display " "))
      (display o)))

;;;
;;;  API
;;;
(define (taco3)                 (taco3-parser tlex error))
(define (taco3-load file)       (with-input-from-file file taco3))
(define (taco3-eval-string str) (with-input-from-string str taco3))

(provide "taco3")
;;; EOF
