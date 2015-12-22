(use gauche.test)
(use ika)

(test-start "taco0")
(use taco0)
(test-module 'taco0)

(define (taco0-compile-string str)
  (display "input: ") (write str)
  (display " => ")
  (let ((ika (with-input-from-string str compile-taco0)))
    (if ika
        (ika/pp ika)
        (print ika))
    ika))

(define taco.out (open-output-file "taco0.out"))

(define (run-test in out)
  (test #"\"~|in|\"" out (lambda ()
                           (with-output-to-port taco.out
                             (lambda ()
                               (taco0-compile-string in))))))

(run-test  "1\n"      '((CONST) 1
                        (RET)
                        ))
(run-test  "-1\n"     '((CONST) 1
                        (NEGATE)
                        (RET)
                        ))
(run-test  "+1\n" #f)             ; this is error 
(run-test  "1 + -2\n"             ; but this is OK
                      '((CONST) 1
                        (PUSH)
                        (CONST) 2
                        (NEGATE)
                        (NUMADD2)
                        (RET)
                        )) 
(run-test  "1.234\n"   '((CONST) 1.234
                         (RET)
                         ))
(run-test  " 1 + 2\n"  '((CONST) 1
                         (PUSH)
                         (CONST) 2
                         (NUMADD2)
                         (RET)
                         ))
(run-test  " 1   2\n" #f)
(run-test  "(1 + 2)*(3+4)\n" '((CONST) 1
                               (PUSH)
                               (CONST) 2
                               (NUMADD2)
                               (PUSH)
                               (CONST) 3
                               (PUSH)
                               (CONST) 4
                               (NUMADD2)
                               (NUMMUL2)
                               (RET)
                               ))

(close-port taco.out)
(test-end :exit-on-failure #t)
;; EOF
