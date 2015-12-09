;;;
;;;
;;;
(define-module ika
  (use gauche.vm.insn)
  (use gauche.vm.code)
  (use vmhack)
  (export ika->vm-code
          vm-dump-code
          vm-code->list
          ))

(select-module ika)
;;;
;;; import useful stuff
;;;
(define-macro (import-from mod . syms)
  `(begin
     ,@(map (lambda (sym) `(define ,sym (with-module ,mod ,sym)))
            syms)))

(define-method vm-insn-code ((info <vm-insn-info>))
  (ref info 'code))

(define-method vm-insn-code ((mnemonic <symbol>))
  (vm-insn-code (vm-find-insn-info mnemonic)))

(define (ika->vm-code ika)
  (let ((ccb (make-compiled-code-builder 0 0 '%toplevel #f #f #f))
        (maxstack 1))
    (for-each (lambda (stmt)
                (case (car stmt)
                  ((CONST)
                   (and (= (length stmt) 2)
                        (integer? (cadr stmt))
                        (compiled-code-emit0o! ccb
                                               (vm-insn-code 'CONST)
                                               (cadr stmt))))
                  ((RET)
                   (compiled-code-emit-RET! ccb))
                  (else (error #"ika: unknown mnemonic: ~(car stmt)"))))
              ika)
    (compiled-code-finish-builder ccb maxstack)
    ccb))

;; EOF
