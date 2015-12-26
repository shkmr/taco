(use gauche.test)
(test-start "taco2")
(use taco2)
(test-module 'taco2)

(define taco.out (open-output-file "taco2.out"))

(define *undef* (if #f #t))

(define (run-test in out)
  (test in out (lambda ()
                 (with-output-to-port taco.out
                   (lambda ()
                     (guard (e ((is-a? e <error>)
                                (cons 'ERROR (ref e 'message)))
                               (else
                                (error "Unexpected exception")))
                       (taco2-eval-string in)))))))

(define simple1 "\
# Simple stmt
{
 1
 2
 3
}
")

(define simple2 "\
{ 1 2 3 }
")

(define simple3 "\
{ 1 2

 3 }
")
(define simple4 "\
{ 1
  2+3 }
")

(define simple5 "\
{1 2+3}
")

(define print1 "\
print 1
")
(define print2 "\
print \"hello, world\n\"
")

(define print3 "\
print 1,2,3
")

(define print4 "\
A=1
B=2
print \"A=\", A, \", B=\", B, \"\n\"
")

(test-section "simple expr")
(run-test "1\n"              1)
(run-test "1+1\n"            2)
(run-test "(1+2)*3\n"        9)
(run-test "3*(3+2*(4+5))\n" 63)
(run-test simple1            3)
(run-test simple2            3)
(run-test simple3            3)
(run-test simple4            5)
(run-test simple5            5)

(test-section "funcall. check taco2.out for compiled codes.")
(run-test print1            *undef*)
(run-test print2            *undef*)
(run-test print3            *undef*)
(run-test "A\n"             '(ERROR . "unknown global A\n"))
(run-test print4            *undef*)
(run-test "A\n"             1)

(test-section "conditional. check taco2.out for compiled codes.")
(run-test "if (1==0) 20\n"  #f)
(run-test "if (1!=0) 10\n"  10)
(run-test "if (1!=1) 10 else 20\n"  20)
(run-test "if (1==1) 10 else 20\n"  10)
(run-test "if (1==0) print(20)\n"  #f)
(run-test "if (1!=0) print(10)\n"  *undef*)
(run-test "if (1!=1) print(10) else print (20)\n"  *undef*)
(run-test "if (1==1) print(10) else print (20)\n"  *undef*)

(test-section "function. check taco2.out for compiled codes.")
(run-test "\
func fact(1) {
  if ($1 == 0) { 
     return  1
  } else {
     return  $1*fact($1 - 1)
  }
}

fact(5)
" 120)

(run-test "\
func fact2(1) {
  if ($1 == 0) return 1 else return $1*fact($1 - 1)
}

fact2(5)
" 120)

(run-test "\
func ack(2) {
     print $1, \" \", $2, \"\\n\"
     if ($1 == 0) return $2+1
     if ($2 == 0) return ack($1-1, 1)
     return ack($1-1, ack($1, $2-1))
}

ack(2,2)
" 7)

(run-test "\
func tak(3) {
  print $1, \" \", $2, \" \" , $3,\"\\n\"
  if ($1 <= $2) {
    return $3
  } else {
    return tak(tak($1-1, $2, $3), tak($2-1, $3, $1), tak($3-1, $1, $2))
  }
}

tak(7,5,3)
" 4)
          
(close-port taco.out)
;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF
