;;;
;;;
;;;
(use gauche.vm.insn)

(define (insn/pp name&info)
  (let ((name (car name&info))
        (info (cdr name&info)))
    (if (not (eq? name (~ info'name)))
      (error "name does not match: " name (~ info'name)))
    (print (if (~ info'obsoleted) "<tr bgcolor=aaaaaa>" "<tr>")
           "<td><code>" (~ info'name) "</code>"
           "<td>" (~ info'obsoleted)
           "<td>" (~ info'num-params)
           "<td>" (~ info'alt-num-params)
           "<td>" (~ info'operand-type)
           "<td>" (if (~ info'combined) "#t" "#f")
           "<td>" (~ info'fold-lref)
           "<td><pre>" (~ info'body) "</pre>"
           "</tr>")
    ))

(print "<table border=1>")
(print "<tr><th>&nbsp;&nbsp;name&nbsp;&nbsp;<th>o?<th>p<th>ap<th>ot<th>c<th>fl<th>body</tr>")
(for-each insn/pp (reverse (class-slot-ref <vm-insn-info> 'all-insns)))
(print "</table>")

;; EOF
