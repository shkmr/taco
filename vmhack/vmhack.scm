;;;
;;; vmhack
;;;

(define-module vmhack
  (use gauche.vm.code)
  (use gauche.vm.insn)
  (export-all)
  )
(select-module vmhack)
(dynamic-load "vmhack")

;;;
;;; from Gauche/src/compile.scm
;;;
(define-macro (define-insn-constants)
  (let1 name&codes
      (map (^[insn] (cons (car insn) (ref (cdr insn)'code)))
           (class-slot-ref <vm-insn-info> 'all-insns))
    `(begin
       ,@(map (^[n&c] `(define-constant ,(car n&c) ,(cdr n&c)))
              name&codes)
       (define-constant .insn-alist. ',name&codes)
       )))
(define-insn-constants)

(define-macro (import-from mod . syms)
  `(begin
     ,@(map (lambda (sym) `(define ,sym (with-module ,mod ,sym)))
            syms)))
         
(import-from gauche.internal
             make-compiled-code-builder
             compiled-code-finish-builder
             compiled-code-emit0o!
             compiled-code-emit-RET!)
;; EOF


