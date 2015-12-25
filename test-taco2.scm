(use gauche.test)
(test-start "taco2")
(use taco2)
(test-module 'taco2)

(define (taco2-load file)       (with-input-from-file file taco2))
(define (taco2-eval-string str) (with-input-from-string str taco2))

(define taco.out (open-output-file "taco2.out"))

(define *undef* (if #f #t))

(define (run-test in out)
  (test in out (lambda ()
                 (with-output-to-port taco.out
                   (lambda ()
                     (taco2-eval-string in))))))


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
(run-test print4            *undef*)

(test-section "conditional. check taco2.out for compiled codes.")
(run-test "if (1==0) 20\n"  #f)
(run-test "if (1!=0) 10\n"  10)
(run-test "if (1!=1) 10 else 20\n"  20)
(run-test "if (1==1) 10 else 20\n"  10)
(run-test "if (1==0) print(20)\n"  #f)
(run-test "if (1!=0) print(10)\n"  *undef*)
(run-test "if (1!=1) print(10) else print (20)\n"  *undef*)
(run-test "if (1==1) print(10) else print (20)\n"  *undef*)

(close-port taco.out)
;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF
