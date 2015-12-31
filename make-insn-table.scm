;;;
;;;
;;;
(use gauche.vm.insn)

(print "<html>
<head>
<meta charset=\"utf-8\">
<title>Gauche VM quick reference</title>
</head>
</body>
<h1>Gauche VM quick rerefernce</h1>
<p>
<font style=\"background-color:#aa7777\">This color</font>
is for combined insns and
<font style=\"background-color:#aaaaaa\">this color</font>
is for obsoleted insns.
</p>
<p>
<code>CONST</code> and <code>LREF</code> are
given special treatment in CompiledCodeBuilder.
</p>
See src/libcode.scm src/code.c src/vminsn.c for details.</p>
")

(print "<table border=1>")
(print "\
<tr><th>name
<th>obsoleted
<th>num-params
<th>alt-num-params
<th>operand type
<th>combined
<th>fold-lref
</tr>
")

(define (insn/tr name&info)

  (let ((name (car name&info))
        (info (cdr name&info)))

    (if (not (eq? name (~ info'name)))
      (error "name does not match: " name (~ info'name)))

    (print (cond ((~ info'obsoleted) "<tr bgcolor=#aaaaaa>")
                 ((~ info'combined)  "<tr bgcolor=#aa7777>")
                 (else               "<tr>"))

           "<td><code>"        (~ info'name) "</code>"
           "<td align=CENTER>" (~ info'obsoleted)
           "<td align=CENTER>" (~ info'num-params)
           "<td align=CENTER>" (~ info'alt-num-params)
           "<td>"              (~ info'operand-type)
           "<td>"              (if (~ info'combined) "#t" "#f")
           "<td>"              (~ info'fold-lref)
           "</tr>"
           )
    ))
(for-each insn/tr (reverse (class-slot-ref <vm-insn-info> 'all-insns)))

(print "</table>")

(print "
<h2>Useful domcuments</h2>
<ul>
<li> <a href=\"http://practical-scheme.net/docs/stack-j.html\">Schemeの実装におけるスタックフレーム(Draft)</a>
<li> <a href=\"http://practical-scheme.net/gauche/memo-j.html\">Gauche/memo-j</a>
<li> <a href=\"http://practical-scheme.net/gauche/memo-stack-j.html\">VMのスタック操作 (未完)</a>
<li> <a href=\"http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3AVMの最適化\">VMの最適化</a>
</ul>
")

(print "
</body>
</html>
")
;; EOF
