;;; -*-Scheme-*-
(begin
  (add-load-path  "vmhack")
  (add-load-path ".")
  (use ika)
  (define c (with-module ika c))

(c '(define (fact n)
      (if (= n 1)
        1
        (* n (fact (- n 1))))))

(c '(define (hello)
      (print "hello, world")))


(c '(define (fo a)
      (if (eq? a 'fobar)
        'yes
        'no)))
