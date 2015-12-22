;;;
;;; taco0 : based on example of lalr.scm
;;;
(define-module taco0
  (use srfi-1)
  (use lalr)
  (export compile-taco0))

(select-module taco0)

(define taco0-parser
  (lalr-parser
   (expect:    0)

   ;; --- token definitions
   (ID NUM = LPAREN RPAREN NEWLINE COMMA
    (left: + -)
    (left: * /)
    (nonassoc: uminus))

   (lines    (lines line) : (return $2)
	     (line)       : (return $1))

   ;; --- rules
   (line     (expr   NEWLINE)        :  (gen/op001 'RET $1)
             (NEWLINE)               :  #f
	     (error  NEWLINE)        :  #f)
   
   (expr     (expr + expr)           : (gen/op002 'NUMADD2 $1 $3)
	     (expr - expr)           : (gen/op002 'NUMSUB2 $1 $3)
	     (expr * expr)           : (gen/op002 'NUMMUL2 $1 $3)
	     (expr / expr)           : (gen/op002 'NUMDIV2 $1 $3)
	     (- expr (prec: uminus)) : (gen/op001 'NEGATE  $2)
	     (NUM)                   : (gen/op011 'CONST $1)
	     (LPAREN expr RPAREN)    : $2)
   ))

;;;
;;;   Code emitter
;;;
(define (gen/op011 op r1)
  `((,op) ,r1))


(define (gen/op001 op r1)
  `(,@r1 
    (,op)))

(define (gen/op002 op r1 r2)
  `(,@r1 
    (PUSH) 
    ,@r2 
    (,op)))

;;;
;;;   The lexer
;;;
(define (make-lexer errorp)
  (lambda ()
    (letrec ((skip-spaces
	      (lambda ()
		(let loop ((c (peek-char)))
		  (if (and (not (eof-object? c))
			   (or (char=? c #\space) (char=? c #\tab)))
		      (begin
			(read-char)
			(loop (peek-char)))))))   
	     (read-number 
	      (lambda (l)
		(let ((c (peek-char)))
                  (if (and (char? c)
                           (or (char-numeric? c)
                               (char=? c #\.)))
                      (read-number (cons (read-char) l))
                      (string->number (apply string (reverse l)))))))
	     (read-id
	      (lambda (l)
		(let ((c (peek-char)))
		  (if (and (char? c) (char-alphabetic? c))
		      (read-id (cons (read-char) l))
		      (string->symbol (apply string (reverse l))))))))

      ;; -- skip spaces
      (skip-spaces)

      ;; -- read the next token
      (let loop ((c (read-char)))
	(cond
	 ((eof-object? c)      '*eoi*)
	 ((char=? c #\newline) 'NEWLINE)
	 ((char=? c #\+)       '+)
	 ((char=? c #\-)       '-)
	 ((char=? c #\*)       '*)
	 ((char=? c #\/)       '/)
	 ((char=? c #\=)       '=)
	 ((char=? c #\,)       'COMMA)
	 ((char=? c #\()       'LPAREN)
	 ((char=? c #\))       'RPAREN)
	 ((char-numeric? c)    (cons 'NUM (read-number (list c))))
	 ((char-alphabetic? c) (cons 'ID  (read-id (list c))))
	 (else                 
	  (errorp "PARSE ERROR : illegal character: " c)
	  (skip-spaces)
	  (loop (read-char))))))))

;;;
;;;   The main program
;;;
(define return #f)

(define (compile-taco0)
  (define (start) (taco0-parser (make-lexer print) print))
  (call/cc (lambda (k)
             (set! return k)
             (start))))
;;; EOF
