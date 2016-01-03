;;
;;             === 楽しい ika プログラミング ===
;;

;;; 1.  ika とは
;;;
;;;   機械語のプログラミングは実行したら即暴走するプログラムが簡単に書けます。
;;;   ika はそんな刺激的な火遊びをしてみたい、いけない子のアセンブラです。
;;;
;;;   ika プログラミングをしていると通常滅多に御目にかかることのない VM
;;;   レジスタのダンプリストなどを拝むことができます。
;;;

;;; 2. 遊び方
;;;
;;;   まずはコマンドラインにて、
;;;
;;;       $ git clone shkmr.github.gom/taco
;;;       $ cd taco
;;;       $ make
;;;
;;;   すると vmhack モジュール、insn.html ができます。
;;;
;;;   次に、Emacs で ika-intro.scm (このファイルのソース)を開き
;;;   M-x run-scheme (もちろん gosh が起動しますよね？ ね！)
;;;   して、順番に C-x C-e して様子をみてみましょう。

;; まずは読み込み

(begin
  (add-load-path ".")
  (add-load-path "vmhack")
  (use ika)
  (use vmhack))

;; ika->vm-code は ika プログラムをアセンブルして実行可能な
;; <compiled-code> を作ります。

(ika->vm-code '(%toplevel (0 0)
                          (CONST) 2
                          (RET)
                          ))

;; %toplevel (0 0) は後ほど 5. で説明するとして、このプログラムは
;; アキュムレータ(val0)に定数 2 をロードしてリータン、つまり、
;; 単に 2 を返すプログラムです。 これを test1 としておきましょう。

(define test1 (ika->vm-code '(%toplevel (0 0)
                                        (CONST) 2
                                        (RET)
                                        )))

;; <compiled-code> を実行するには vmhack モジュールの
;; vm-code-execute! に渡せばオッケイ。

(vm-code-execute! test1 (interaction-environment))
;; ちゃんと 2 が返ってきました。

;; 第二引数には (interaction-environment) の代わりに
;; モジュールを渡して構いません。 今後変わるかもしれないけど。

(vm-code-execute! test1 (find-module 'user))

;;; 3.  Gauche VM の機械語その1
;;;
;;;    Gauche VM の機械語のフォーマットは次の6種類です。
;;;
;;;      1. OPCODE
;;;      2. OPCODE OBJ
;;;      3. OPCODE ADDR
;;;      4. OPCODE CODE
;;;      5. OPCODE OBJ ADDR
;;;      6. OPCODE CODES
;;;
;;;   OBJ は任意の Scheme オブジェクト、 ADDR はアドレス、 CODE は
;;;   <compiled-code>、 CODES は　<complied-code> のリストです。
;;;   アドレスは Scheme オブジェクトではないので、ika プログラムでは
;;;   label 擬似命令で指定します。
;;;
;;;   OPECODE にはパラメータを一つか二つ取るものがあり、例えば
;;;   パラメータなしの (CONST) に対してパラメータを一つ取る (CONTI n)
;;;   があります。パラメータは小さめの整数でその範囲は1 integer-fits-insn-arg?
;;;   で確認できます。
;;;
;;;   1. の $ make でできた insn.html は機械語の命令(insn)の一覧です。
;;;   Gauche VM には複合命令(combined insn)という、頻出する複数の命令
;;;   を一つにまとめたものがあります。 例えば CONST-PUSH は CONST と PUSH
;;;   を一つにまとめたものです。 ika プログラミングにおいては、これらを直接
;;;   使うこともできますが、ika が呼び出す complied-code-builder (cc_builder) が
;;;   自動的にまとめてくれるので、とりあえずは基本形だけを使っていれば十分
;;;   かと思います。

;; 先ほどの test1 を見てみましょう。

(vm-dump-code test1)

;; リスト形式で

(vm-code->list test1)

;; ika プログラムでは 2語の (CONST) 2 としたところが 1語の (CONSTI 2)
;; に変わっています。

;; また、文字列定数を返すプログラムは

(vm-dump-code (ika->vm-code '(%toplevel (0 0)
                                        (CONST) "abracadabra"
                                        (RET))))

;; のように (CONST) operand (RET) が (CONST-RET) operand に変換さてれます。
;; 今回はオペランドが命令に埋め込める物ではないので代わりに複合命令(CONST-PUSH)
;; になりました。

;;;   機械語は Gauche のコンパイラに教えてもらうのが手っ取り早いと思います。
;;;   ika を use すると Gauche のコンパイラも使えるようになります。
;;;   コンパイラは <compiled-code> を返します。

;; 例えば先ほどの 2 を返すプログラムは

(vm-dump-code (compile 2 (interaction-environment)))

;; vm-code->list で得られるリストに (%toplevel (0 0)) をくっつけると
;; ika プログラムになります。

(ika/pp `(%toplevel (0 0)
             ,@(vm-code->list (compile 2 (interaction-environment)))))
;; ika/pp は ika プログラムを見やすくインデントして表示します。
;; 左端の数字はアドレスです。

;;;   ところで、後ほど 5. で説明しますが、ika プログラムには
;;;   アドレスは不要なので出力は ika プログラムにはなってません。
;;;   これらをまとめた c という関数が ika モジュールの中にあるので
;;;   いろいろ試してみましょう。

;; c は export されてないので、勝手に取り込みます。

(define c (with-module ika c))

(c 2)

(c 654321)

(c "abracadabra")

(c #t)

(c #f)

(c '())

(c '(+ 1 2))

(c '(+ x y))

(c '(print "abracadabra"))

(c '(define (fact n)
      (if (= n 1)
        1
        (* n (fact (- n 1))))))

;;; 4.  <compiled-code>
;;;
;;;   <compiled-code> には次ような(Schemeからアクセス可能な)スロットがあります。
;;;
;;;      parent        ; 親 <compiled-code>
;;;      arg-info      ; ソースコードにおける引数(シンボル、またはシンボルのリストまたは #f)
;;;      info          ; ソースコードの情報、code 内の相対アドレスと対応するソースコードの alist
;;;      required-args ; 必須引数の数
;;;      optional-args ; 省略可能な引数の数
;;;      name          ; 名前、シンボルまたは #f
;;;      full-name     ; 親の名前を含む名前
;;;      size          ; code のワード数
;;;      max-stack     ; 使用するスタックのワード数
;;;      intermediate-form ; inline 展開するときに使われる何か(よく調べてない)
;;;
;;;   機械語のプログラムは vm-code->list で取り出すことができます。
;;;
;;;   ika->vm-code は ika プログラムにしたがってこれらのスロットを設定します。
;;;

;;; 5.  ika の文法その1
;;;
;;;   ika プログラムは
;;;
;;;       (name code-spec body)
;;;
;;;   というリストです。 name はシンボル code-spec は基本的には
;;;
;;;       (required-args optional-args)
;;;
;;;   という形式で、オプションの kv-list でいくつかの <complied-code> スロット
;;;   を設定することができます。(が私自身あまり試してません。)
;;;
;;;       (required-args optional-args :key val ...)
;;;
;;;   現在指定可能なのは :arg-info :parent :intermediate-form です。
;;;
;;;   例えば、先に見た test1 の場合は
;;;
;;;       name       =    %toplevel
;;;       code-spec  =    (0 0)
;;;       body       =    (CONST) 2 (RET)
;;;
;;;   となります。
;;;
;;;   それではもう少し複雑な ika のプログラムを組んでみましょう。

;; 3. の終わりに示した

(c '(print "abracadabra"))

;; に相当する ika プログラムは

(define ika1 `(%toplevel (0 0)
                  (CONST) "abracadabra"
                  (PUSH)
                  (GREF) ,(make-identifier 'print (find-module 'user) '())
                  (TAIL-CALL 1)
                  (RET)))

;; となります。　ここで、(GREF) のオペランドは識別子(identifier)なので
;; make-identifer で user モジュールの識別子を作ってやります。
;; これを ika->vm-code でアセンプルすると

(vm-dump-code (ika->vm-code ika1))

;; となりめでたくコンパイラの出力と同等になりました。
;; ここでは vm-dump-code を使いましたが、もちろん、先ほどと同様に

(ika/pp `(%top-level (0 0) ,@(vm-code->list (ika->vm-code ika1))))

;; としても構いません。

;;;   擬引用は煩わしいので ika では user モジュールの識別子を作る擬似命令 mkid
;;;   を用意しています。(これは将来互換性破る形で変更するかもしれません。)

(define ika2 '(%toplevel (0 0)
                  (CONST) "abracadabra"
                  (PUSH)
                  (GREF) (mkid print)
                  (TAIL-CALL 1)
                  (RET)))
(vm-dump-code (ika->vm-code ika2))

;; 実行してみましょう

(vm-code-execute! (ika->vm-code ika2) (interaction-environment))

;;;   余談ですが、(CONST) (PUSH) -> (CONST-PUSH) などの合成は ika が
;;;   やってるのではなく、Gauche の cc_builder がやってくれてます。
;;;   ありがたや、ありがたや。

;;;   6.  Gauche VM の機械語その2
;;;
;; 次に手続きの定義を見てみましょう。

(c '(define (foo) (print "foo")))
      
;; 手続き本体は <compiled-code> となって (CLOSURE) のオペランドに
;; なっています。その結果クロージャが val0 にロードされます。
;; そして (DEFINE 0) で foo の束縛します。
;; vm-dump-code すると 2つの <complied-code> があるのがわかりやすいです。

(vm-dump-code (compile '(define (foo) (print "foo")) (interaction-environment)))

;;;   DEFINE のパラメータは 0 が通常の define, SCM_BINDING_CONST が
;;;   define-constant、 SCM_BINDING_INLINABLE が define-inline に
;;;   相当するんだと思います。 (まだ、あまり試してません Gauche/src/vminsn.scm 参照)
;;;   ここで重要なのは機械語の DEFINE は Scheme のトップレベルの define
;;;   に相当するということ。 Scheme 手続き内の define はコンパイラによって
;;;   ローカル変数への代入に変換されます。

;; ここでもう一度階乗を見てみましょう。

(c '(define (fact n)
      (if (= n 1)
        1
        (* n (fact (- n 1))))))

;;;   fact には引数が1つあるので (fact (1 0) ...) となってます。
;;;   引き数(ローカル変数)の参照は (LREF depth offset) で行います。
;;;   (LREF0) は (LREF 0 0) に相当するショートカット命令です。
;;;   ローカル変数の指定はスタックフレームをある程度理解しないとできないので、
;;;   insn.html にリンクを張った shiro さんによるドキュメントを
;;;   読んで勉強しましょう。　実のところ、私はさらっと読んだだけだと
;;;   よくわからず、実際に実装してみてようやくどういう構造になっているか
;;;   理解した感じです。(mgvm.scm) 要は時間をかけて一つずつ解きほぐす。

;;;   さて、(BNUMNEI 1) のオペランドはアドレスで相当する位置は
;;;   左側の数字が 5 の (LREF0-PUSH) がある所です。
;;;   (PRE-CALL 1) のオペランドも同様です。
;;;   ika ではアドレスは label 擬似命令で指定します。

;; というわけで、fact を ika で書くとこうなります。

(define fact.ika  '(%toplevel (0 0)
                       (CLOSURE) (ikafact (1 0)
                                     (LREF 0 0)
                                     (BNUMNEI 1) (label 1)
                                     (CONST) 1
                                     (RET)
                          (label 1)  (LREF 0 0)
                                     (PUSH)
                                     (PRE-CALL 1) (label 2)
                                     (LREF 0 0)
                                     (NUMADDI -1)
                                     (PUSH)
                                     (GREF) (mkid ikafact)
                                     (CALL 1) 
                          (label 2)  (NUMMUL2)
                                     (RET))
                       (DEFINE 0) (mkid ikafact)
                       (RET)
                       ))

(vm-dump-code (ika->vm-code fact.ika))

;; 実行すると

(vm-code-execute! (ika->vm-code fact.ika) (interaction-environment))

;; ikafact が定義され、

(ikafact 5)

;; 階乗が計算できました。

;;; おしまい
