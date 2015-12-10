(use gauche.test)
(use ika)

(test-start "taco0")
(use taco0)
(test-module 'taco0)

(define (taco0-compile-string str)
  (let ((ika (with-input-from-string str compile-taco0)))
    (write str)
    (display " => ")
    (if ika
        (ika/pp ika)
        (print ika))
    ika))

#|
   (vm-dump-code (compile '(+ 1.234 a) (interaction-environment)))
|#


(define (run-test in out)
  (test* in out (with-input-from-string in compile-taco0)))

(run-test "1\n"
          '((CONST 1)))
(run-test "-1\n"
          '((CONST 1)
            (NEGATE)))
(run-test "+1\n" #f)             ; this is error 
(run-test "1 + -2\n"             ; but this is OK
          '((CONST 1)
            (PUSH)
            (CONST 2)
            (NEGATE)
            (NUMADD2))) 
(run-test "1.234\n" '((CONST 1.234)))
(run-test " 1 + 2\n"
          '((CONST 1)
            (PUSH)
            (CONST 2)
            (NUMADD2)))
(run-test " 1   2\n" #f)
(run-test "(1 + 2)*(3+4)\n"
          '((CONST 1)
            (PUSH)
            (CONST 2)
            (NUMADD2)
            (PUSH)
            (CONST 3)
            (PUSH)
            (CONST 4)
            (NUMADD2)
            (NUMMUL2)))

(test-end :exit-on-failure #t)
