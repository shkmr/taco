;;
(use mgvm)
;;;
;;;
;;;
(define undef (if #f #t))

(define (test v txt prog expect)
  (let ((result undef))
    (mgvm/init :program prog)
    (print ";;;\n;;; TESTING : " txt "\n;;;")
    (print "\n;;\n;; Initial condition\n;;")
    (mgvm/dump)
    (print ";;\n;; Program start.\n;;")
    (set! result (mgvm/run :verbose v))
    (print "\n;;\n;; finished.\n;;")
    (mgvm/dump)
    (if (equal? result expect)
      (print txt " -> success !!\n")
      (print txt " -> FAIL    !!\n"))
    ))

(define (main args)
  ;;
  ;;   TEST1 : Simple one call
  ;;
  ;;      (print "hello, world")
  ;;
  (define test1
    `((PRE-CALL 1) ("hello, world"
                    (PUSH)
                    (GREF) ,print
                    (CALL 1)
                    )))
  ;;
  ;;  TEST2 : Two calls
  ;;
  ;;       (display "hello, world")
  ;;       (newline)
  ;;
  (define test2
    `((PRE-CALL 1) ("hello, world"
                    (PUSH)
                    (GREF) ,display
                    (CALL 1)
                    )
      (PRE-CALL 0) ((GREF) ,newline
                    (CALL 0))))

  ;;
  ;;  TEST3 : function definition
  ;;
  ;;    (define (foo) (print "hello, world"))
  ;;    (foo)
  ;;
  (define test3
    '(
      (LAMBDA 0 0) ((PRE-CALL 1) ("hello, world"
                                  (PUSH)
                                  (GREF) print
                                  (CALL 1)))
      (DEFINE)  FOO
      (PRE-CALL 0) ((GREF) FOO
                    (CALL 0))))

  ;;
  ;;  TEST4 : TAIL-CALL
  ;;
  (define test4
    '(
      (LAMBDA 0 0) ((PRE-TAIL 1) 
                    "hello, world"
                    (PUSH)
                    (GREF) print
                    (TAIL-CALL 1))
      (DEFINE) FOO
      (PRE-CALL 0) ((GREF) FOO
                    (CALL 0))
      ))

  ;;
  ;;  TEST5 : LET
  ;;  (define (FOO X)
  ;;     (let ((Y 1)
  ;;        (+ X Y)))
  (define test5
    '(
      (LAMBDA 1 0) ((LET 1) (1
                             (LSET0)
                             (LREF10-PUSH)
                             (LREF0)
                             (NUMADD2)))
      (DEFINE) FOO
      (PRE-CALL 1) (3
                    (PUSH)
                    (GREF) FOO
                    (CALL 1))))

  ;;   TEST7
  ;;   (define (fact n)
  ;;      (if (= n 0)
  ;;         1
  ;;         (* n (fact (- n 1)))))
  ;;   (fact 5)
  ;;
  (define test7
    '(
      (LAMBDA 1 0) ( 
                    ;; (PRE-CALL 1) ((LREF0-PUSH)
                    ;;   (GREF) ,print
                    ;;   (CALL 1))
                    (LREF0-PUSH)
                    0
                    (NUMEQ2)
                    (IF) (1)
                    (PRE-CALL 2) ((LREF0-PUSH)
                                  (PRE-CALL 1) ((LREF0)
                                                (NUMADDI -1)
                                                (PUSH)
                                                (GREF) FACT
                                                (CALL 1))
                                  (PUSH)
                                  (GREF) *
                                  (CALL 2)))
      (DEFINE)
      FACT
      (PRE-CALL 1) ((PUSHI 5)
                    (GREF) FACT
                    (CALL 1))
      ))

  (define test8
    '(
      (LAMBDA 1 0) (
                    ;; (PRE-CALL 1) ((LREF0-PUSH)
                    ;;   (GREF) ,print
                    ;;   (CALL 1))
                    (LREF0-PUSH)
                    0
                    (NUMEQ2)
                    (IF) (1)
                    (PRE-TAIL 2)
                    (LREF0-PUSH)
                    (PRE-CALL 1) ((LREF0)
                                  (NUMADDI -1)
                                  (PUSH)
                                  (GREF) FACT
                                  (CALL 1))
                    (PUSH)
                    (GREF) *
                    (TAIL-CALL 2))
      (DEFINE)
      FACT
      (PRE-CALL 1) ((PUSHI 5)
                    (GREF) FACT
                    (CALL 1))
      ))
  ;;
  ;; (define (sum n)
  ;;    (letrec ((lp (lambda (n s)
  ;;                 (if (= n 0)
  ;;                     s
  ;;                     (lp (- n 1) (+ s n))))))
  ;;    (lp n 0)))
  ;;
  (define test10 
    '((LAMBDA 1 0) ((LET 1) ((LAMBDA 2 0) (
                                           (LREF1-PUSH) ; n
                                           0
                                           (NUMEQ2)
                                           (IF) ((LREF0))
                                           (PRE-CALL 2) (
                                                         (LREF1) ; n
                                                         (NUMADDI -1)
                                                         (PUSH) ; (- n 1)
                                                         (LREF0-PUSH) ; s
                                                         (LREF1) ; n
                                                         (NUMADD2)
                                                         (PUSH) ; (+ s n)
                                                         (LREF10)
                                                         (CALL 2)))
                             (LSET0)
                             (PRE-CALL 2) (
                                           (LREF10-PUSH) ; n
                                           (PUSHI 0)     ; 0
                                           (LREF0)
                                           (CALL 2))))
      (DEFINE)
      SUM
      (PRE-CALL 1) (5
                    (PUSH)
                    (GREF) SUM
                    (CALL 1))
      ))

  (define test11
    '(
      (LAMBDA 1 0) ((LET 1) ((LAMBDA 2 0) ((LREF1-PUSH)
                                           0
                                           (NUMEQ2)
                                           (IF) ((LREF0))
                                           (PRE-TAIL 2)
                                           (LREF1)
                                           (NUMADDI -1)
                                           (PUSH)
                                           (LREF0-PUSH)
                                           (LREF1)
                                           (NUMADD2)
                                           (PUSH)
                                           (LREF10)
                                           (TAIL-CALL 2)
                                           )
                             (LSET0)
                             (PRE-TAIL 2)
                             (LREF10-PUSH)
                             (PUSHI 0)
                             (LREF0)
                             (TAIL-CALL 2)
                             )
                    )
      (DEFINE) SUM
      (PRE-CALL 1) (5
                    (PUSH)
                    (GREF) SUM
                    (CALL 1))
      ))

  (let ((v (> (length args) 1)))
    (test v "TEST1: Simple call" test1  undef)
    (test v "TEST2: Two calls"   test2  undef)
    (test v "TEST3: DEFINE"      test3  undef)
    (test v "TEST4: TAIL-CALL"   test4  undef)
    (test v "TEST5: LET"         test5  4)
    (test v "TEST7: FACT"        test7  120)
    (test v "TEST8: FACT TAIL"   test8  120)
    (test v "TEST10: SUM"        test10 15)
    (test v "TEST11: SUM TAIL"   test11 15)
    )
  0
  ) ; END-OF-MAIN

