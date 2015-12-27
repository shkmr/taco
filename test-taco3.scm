(use gauche.test)
(test-start "taco3")
(use taco3)
(test-module 'taco3)

(define taco.out (open-output-file "taco3.out"))

(define *undef* (if #f #t))

(define (run-test in expected)
  (test in expected (lambda ()
                 (with-output-to-port taco.out
                   (lambda ()
                     (guard (e ((is-a? e <error>)
                                (cons 'ERROR (ref e 'message)))
                               (else
                                (error "Unexpected exception")))
                       (print "***********************************************************")
                       (display "input: ")
                       (write in)
                       (newline)
                       (let* ((val  (taco3-eval-string in))
                              (ika (with-module taco3 *ika*)))
                         (list ika val))))))))

(newline)
(test-section "constant expr")
(run-test "1\n"             '((%top-level (0 0)
                                          (CONST) 1
                                          (RET))
                              1))

(run-test "1+1\n"           '((%top-level (0 0)
                                          (CONST) 2
                                          (RET))
                              2))

(run-test "1.5-1\n"         '((%top-level (0 0)
                                          (CONST) 0.5
                                          (RET))
                              0.5))

(run-test "1-1.5\n"         '((%top-level (0 0)
                                          (CONST) -0.5
                                          (RET))
                              -0.5))

(run-test "2.5-1.2\n"       '((%top-level (0 0)
                                          (CONST) 1.3
                                          (RET))
                              1.3))

(run-test "(1+2)*(4 + 3)\n" '((%top-level (0 0)
                                          (CONST) 21
                                          (RET))
                              21))

(run-test "(4+2)/(1+1)\n"   '((%top-level (0 0)
                                          (CONST) 3
                                          (RET))
                              3))

(newline)
(test-section "referring global variable.")
(test* "Make sure A is not bound."  (test-error <error>) A
       (lambda (expected actual)
         (string-scan (slot-ref actual 'message) "unbound variable:")))

(run-test "A=2.0\n"
          `((%top-level (0 0)
                        (CONST) 2.0
                        (DEFINE 0) (mkid A)
                        (RET))
            A))

(test* "Now A is bound."  2.0 A)

(run-test "A\n"              `((%top-level (0 0)
                                           (GREF) (mkid A)
                                           (RET))
                               ,A))

(run-test "A+1\n"            `((%top-level (0 0)
                                           (GREF) (mkid A)
                                           (NUMADDI 1)
                                           (RET))
                               ,(+ A 1)))


(run-test "1+A\n"            `((%top-level (0 0)
                                           (GREF) (mkid A)
                                           (NUMADDI 1)
                                           (RET))
                               ,(+ 1 A)))

(run-test "1-A\n"            `((%top-level (0 0)
                                           (GREF) (mkid A)
                                           (NUMSUBI 1)
                                           (RET))
                               ,(- 1 A)))

(run-test "A-1\n"            `((%top-level (0 0)
                                           (GREF) (mkid A)
                                           (NUMADDI -1)
                                           (RET))
                               ,(- A 1)))

(run-test "A+1.0\n"           `((%top-level (0 0)
                                            (GREF) (mkid A)
                                            (PUSH)
                                            (CONST) 1.0
                                            (NUMADD2)
                                            (RET))
                                ,(+ A 1.0)))


(run-test "1.0+A\n"           `((%top-level (0 0)
                                            (CONST) 1.0
                                            (PUSH)
                                            (GREF) (mkid A)
                                            (NUMADD2)
                                            (RET))
                                ,(+ 1.0 A)))

(run-test "-1\n"              `((%top-level (0 0)
                                            (CONST) -1
                                            (RET))
                                -1))

(run-test "-1.0\n"             `((%top-level (0 0)
                                             (CONST) -1.0
                                             (RET))
                                 -1.0))

(run-test "-A\n"              `((%top-level (0 0)
                                            (GREF) (mkid A)
                                            (NEGATE)
                                            (RET))
                                ,(- A)))

(run-test "A-1.0\n"           `((%top-level (0 0)
                                            (GREF) (mkid A)
                                            (PUSH)
                                            (CONST) 1.0
                                            (NUMSUB2)
                                            (RET))
                                ,(- A 1.0)))

(run-test "1.0-A\n"           `((%top-level (0 0)
                                            (CONST) 1.0
                                            (PUSH)
                                            (GREF) (mkid A)
                                            (NUMSUB2)
                                            (RET))
                                ,(- 1.0 A)))

(run-test "1.0*A\n"           `((%top-level (0 0)
                                            (CONST) 1.0
                                            (PUSH)
                                            (GREF) (mkid A)
                                            (NUMMUL2)
                                            (RET))
                                ,(* 1.0 A)))

(run-test "1.0/A\n"           `((%top-level (0 0)
                                            (CONST) 1.0
                                            (PUSH)
                                            (GREF) (mkid A)
                                            (NUMDIV2)
                                            (RET))
                                ,(/ 1.0 A)))

(run-test "2^3\n"             '((%top-level (0 0)
                                            (CONST) 8
                                            (RET))
                                8))

(run-test "A^3\n"             '((%top-level (0 0)
                                            (PRE-CALL 2) (label 1)
                                            (GREF) (mkid A)
                                            (PUSH)
                                            (CONST) 3
                                            (PUSH)
                                            (GREF) (mkid expt) ; become GREF-CALL!
                                            (CALL 2)
                                            (label 1)
                                            (RET))
                                8.0))

(close-port taco.out)

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF