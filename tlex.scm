;;;
;;; LEXER FOR TACO1 AND TACO2
;;;
(add-load-path "../lang.tool")
(load "c-lex.scm")

(define c-keywords
  '(
    (break       BREAK)		
    (case        CASE)		
    (continue	 CONTINUE)
    (default	 DEFAULT)
    (do		 DO)
    (else	 ELSE)
    (enum	 ENUM)		
    (for	 FOR)
    (func        FUNC)
    (goto	 GOTO)
    (if		 IF)	
    (print       PRINT)
    (proc        PROC)
    (return	 RETURN)
    (switch	 SWITCH)
    (var         VAR)
    (while	 WHILE)

    ))

(define c-operators
  '((>>=         (ASSIGN . >>=))
    (<<=         (ASSIGN . <<=))
    (+=          (ASSIGN . +=))
    (-=          (ASSIGN . -=))
    (*=          (ASSIGN . *=))
    (/=          (ASSIGN . /=))
    (%=          (ASSIGN . %=))
    (&=          (ASSIGN . &=))
    (^=          (ASSIGN . ^=))
    (=           =)
;;  (|=          (ASSIGN . |=))  ; scheme
    (>>          RSHIFT)
    (<<          LSHIFT)
    (++          PLUSPLUS)
    (--          MINUSMINUS)
    (&&          &&)
;;  (||          OROR)           ; scheme
    (<=          <=)
    (>=          >=)
    (<           <)
    (>           >)
    (==          ==)
    (!=          !=)
    (+           +)
    (-           -)
    (*           *)
    (/           /)
    (%           %)
    (&           &)
    (^           ^)
    (?           ?)
    ))

;;;
;;; T-LEX. 
;;;
;;; Difference from c-lex.
;;;
;;;  1. Return NEWLINE token.
;;;  2. $n ARG token.
;;;

(define (t-lex)
  (skip-spaces)
  (let loop ((c (read-char)))
    (cond
     ((eof-object? c)  '*eoi*)
     ((char=? c #\newline) 
      (inc! lineno)
      'NEWLINE)
     ((char=? c #\#)                    ; XXX Check beginning of line?
      (do-sharp-command)
      (loop (read-char)))

     ((char=? c #\$)
      (cons 'ARG (- (char->integer (read-char))
                    (char->integer #\0))))

     ((char=? c #\0)
      (if (char-ci=? (peek-char) #\x)
          (begin (read-char)
                 (cons 'CONSTANT (read-hexadecimal '())))
          (cons 'CONSTANT (read-octal (list c)))))

     ((char-numeric? c)
      (cons 'CONSTANT (read-decimal (list c))))

     ((and (char=? c #\.) (char-numeric? (peek-char)))
      (cons 'CONSTANT (read-flonum (list c))))

     ((char=? c #\.)  
      (if (char=? (peek-char) #\.)
          (begin
            (read-char)
            (if (char=? (peek-char) #\.)
                (begin
                  (read-char)
                  'ELLIPSIS)
                (error "syntax error ..")))
          'DOT))

     ((and (char=? c #\L) (char=? (peek-char) #\"))
      (read-char)                       ; L
      (cons 'STRING   (read-string-literal)))
     ((and (char=? c #\L) (char=? (peek-char) #\'))
      (read-char)                       ; L
      (cons 'CONSTANT (list 'wchar (read-character-constant))))
     ((char=? c #\")
      (cons 'STRING   (read-string-literal)))
     ((char=? c #\')
      (cons 'CONSTANT (list 'int (read-character-constant))))

     ((char-set-contains? initial-identifier-charset c)
      (read-identifier (list c)))

     ((char-set-contains? operator-charset c)
      (read-operator c))

     ((char=? c #\,)   'COMMA)
     ((char=? c #\:)   'COLON)
     ((char=? c #\;)   'SEMICOLON)
     ((char=? c #\()   'LPAREN)
     ((char=? c #\))   'RPAREN)
     ((char=? c #\{)   'LCBRA)
     ((char=? c #\})   'RCBRA)
     ((char=? c #\[)   'LSBRA)
     ((char=? c #\])   'RSBRA)

     ;; special case for c-operator due to Scheme
     ((char=? c #\|)   (or (follow #\| 'OROR          #f)
                           (follow #\= '(ASSIGN . OR) #f)
                           'OR))
     (else
      (error "waring: illegal character: " c)
      (skip-spaces)
      (loop (read-char))))))
;;;;;;
;;;;;; END OF T-LEX
