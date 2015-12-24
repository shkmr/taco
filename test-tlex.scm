(use gauche.test)
(test-start "tlex")
(use tlex)
(test-module 'tlex)

(define *eof* (with-input-from-string "" read-char))

(define (run-lexer)
  (guard (e ((is-a? e <error>)
             (cons 'ERROR (ref e 'message)))
            (else
             (error "Unexpected exception")))
    (port-map (lambda (x) x)
              (lambda ()
                (let ((x (tlex)))
                  (if (eq? x '*eoi*)
                    *eof*
                    x))))))

(define (test-tlex in expect)
  (test* in expect (with-input-from-string in run-lexer)))

;;
;;  TESTS START HERE
;;

;;
;; CARM 2.3 TOKENS
;;
(test-section "CARM 2.3 tokens")
(test-tlex "forwhile;" '((IDENTIFIER . forwhile) SEMICOLON))
(test-tlex "b>x;"   '((IDENTIFIER . b) >    (IDENTIFIER . x) SEMICOLON))
(test-tlex "b->x;"  '((IDENTIFIER . b) PTR_EXTENT   (IDENTIFIER . x) SEMICOLON))
(test-tlex "b--x;"  '((IDENTIFIER . b) MINUSMINUS   (IDENTIFIER . x) SEMICOLON))
(test-tlex "b---x;" '((IDENTIFIER . b) MINUSMINUS - (IDENTIFIER . x) SEMICOLON))
;;
;; integer constants
;;
(test-section "integer constants")
(test-tlex "1234;"    '((CONSTANT int "1234") SEMICOLON))
(test-tlex "012;"     '((CONSTANT int "012")  SEMICOLON))
(test-tlex "0x12;"    '((CONSTANT int "0x12") SEMICOLON))
;;
;; character constants
;;
(test-section "character constants")
(test-tlex "'a';"     '((CONSTANT int "97")   SEMICOLON))
(test-tlex "'A';"     '((CONSTANT int "65")   SEMICOLON))
(test-tlex "' ';"     '((CONSTANT int "32")   SEMICOLON))
(test-tlex "'?';"     '((CONSTANT int "63")   SEMICOLON))
(test-tlex "'\\r';"   '((CONSTANT int "13")   SEMICOLON))
(test-tlex "'\\0';"   '((CONSTANT int "0")    SEMICOLON))
(test-tlex "'\"';"    '((CONSTANT int "34")   SEMICOLON))
;; "255" or "-1", depending on the size of char
(test-tlex "'\\377';" '((CONSTANT int "255")  SEMICOLON))
(test-tlex "'%';"     '((CONSTANT int "37")   SEMICOLON))
(test-tlex "'\\23';"  '((CONSTANT int "19")   SEMICOLON))
(test-tlex "'8';"     '((CONSTANT int "56")   SEMICOLON))
(test-tlex "'\\\\';"  '((CONSTANT int "92")   SEMICOLON))
;; (string->number "41424344" 16) -> 1094861636
(test-tlex "'ABCD';"  '((CONSTANT int "1094861636") SEMICOLON))
;;
;; floating point constants
;;
(test-section "floating point constants")
(test-tlex "0.;"       '((CONSTANT double "0.")       SEMICOLON))
(test-tlex "3e1;"      '((CONSTANT double "3e1")      SEMICOLON))
(test-tlex "3.14159;"  '((CONSTANT double "3.14159")  SEMICOLON))
(test-tlex ".0;"       '((CONSTANT double ".0")       SEMICOLON))
(test-tlex "1.0E-3;"   '((CONSTANT double "1.0E-3")   SEMICOLON))
(test-tlex "1e-3;"     '((CONSTANT double "1e-3")     SEMICOLON))
(test-tlex "1.0;"      '((CONSTANT double "1.0")      SEMICOLON))
(test-tlex "0.00034;"  '((CONSTANT double "0.00034")  SEMICOLON))
(test-tlex "2e+9;"     '((CONSTANT double "2e+9")     SEMICOLON))
;; STDC floating point
(test-tlex "1.0f;"     '((CONSTANT float "1.0")          SEMICOLON))
(test-tlex "1.0e67L;"  '((CONSTANT long-double "1.0e67") SEMICOLON))
(test-tlex "0E1L;"     '((CONSTANT long-double "0E1")   SEMICOLON))
(test-tlex "0x1.0p1;"  '((CONSTANT double "0x1.0p1") SEMICOLON))
(test-tlex "0x1.0;"    '(ERROR . "hexadecimal floating constants require an exponent"))
;;
;; string constants
;;
(test-section "string constants")
(test-tlex "\"\";" '((STRING . "") SEMICOLON))
(test-tlex "\"\\\"\";" '((STRING . "\"") SEMICOLON))
(test-tlex "\"Copyright 2000 \\\nTexas Instruments. \""
           '((STRING . "Copyright 2000 Texas Instruments. ")))
(test-tlex "\"Comments begin with '/*'.\\n\""
           '((STRING . "Comments begin with '/*'.\n")))
(test-tlex "1.37E+6L;" '((CONSTANT long-double "1.37E+6") SEMICOLON)          )
(test-tlex "\"String \"\"FOO\"\"\""  '((STRING . "String ") (STRING . "FOO") (STRING . "")) )
(test-tlex "\"Strinng+\\\"FOO\\\"\"" '((STRING . "Strinng+\"FOO\""))          )


(test-section "other")
(test-tlex "X++Y;"     '((IDENTIFIER . X) PLUSPLUS (IDENTIFIER . Y) SEMICOLON))
(test-tlex "-12ul;"    '(- (CONSTANT unsigned-long "12") SEMICOLON)           )
(test-tlex "x**2;"     '((IDENTIFIER . x) * * (CONSTANT int "2") SEMICOLON)   )
;;Trigraphs are supposed to be processed in prior to lexer
;;(test-tlex "\"X??/\"" '())
;;Dollar sign canot be a part of identifier
;;(test-tlex "B$C;" '())
(test-tlex "A*=B;"  '((IDENTIFIER . A) (ASSIGN . *=) (IDENTIFIER . B) SEMICOLON) )
;; ## operator is processed by cpp, lexer is not expected to see it.
;;(test-tlex "while##DO;" '())

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF
