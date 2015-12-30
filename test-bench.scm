(use gauche.time)
(use taco3)

(display "******************** Benchmark test ********************\n")
(display "Running benchmark test...\n" (current-error-port))

(define (stak x y z)
  (if (<= x y)
    z
    (stak (stak (- x 1) y z)
          (stak (- y 1) z x)
          (stak (- z 1) x y))))

(taco3-eval-string "
    func ttak(3) {
      if ($1 <= $2) {
        return $3
      } else {
        return ttak(ttak($1-1, $2, $3), ttak($2-1, $3, $1), ttak($3-1, $1, $2))
      }
    }
")

;; Use (BOX) for all lvars blindly...
(with-module taco3 (define find-mutated-lvar find-mutated-lvar-0))

(taco3-eval-string "
    func utak(3) {
      if ($1 <= $2) {
        return $3
      } else {
        return utak(utak($1-1, $2, $3), utak($2-1, $3, $1), utak($3-1, $1, $2))
      }
    }
")

(time-these/report '(cpu 1.0) `((stak . ,(lambda () (stak 10 5 1)))
                                (ttak . ,(lambda () (ttak 10 5 1)))
                                (utak . ,(lambda () (utak 10 5 1)))
                                ))

;; EOF
