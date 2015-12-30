;;;
;;; vmhack
;;;
(define-module vmhack
  (export vm-code-execute!
          compiled-code-required-args
          compiled-code-optional-args
          compiled-code-args
          compiled-code-name
          compiled-code-code-size
          compiled-code-maxstack
          ))
(select-module vmhack)
(dynamic-load "vmhack")
;; EOF
