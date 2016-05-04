(define-syntax cating
  (syntax-rules ()
    [(cating (str . args))
     (format #f str . args)]
    [(cat (str . args) (str2 . args2) ...)
     (string-append
      (cating (str . args))
      (cating (str2 . args2) ...))]))

(define-syntax cat
  (syntax-rules ()
    [(cat e ...)
     (format #t (cating e ...))]))

(define-syntax orz
  (syntax-rules ()
    [(orz who c ...)
     (error who (cating ("~%") c ...))]))

(define-syntax type
  (syntax-rules ()
    [(type . body)
     (void)]))

(define-syntax :
  (syntax-rules ()
    [(: . body)
     (void)]))

(define-syntax test
  (syntax-rules ()
    [(test b1 b2)
     (if (equal? b1 b2)
       #t
       (let ()
         (cat ("\n"))
         (cat ("<test-fail-report-begin>\n"))
         (cat ("<actual-form> :\n"))
         (pretty-print (quote b1))
         (cat ("<actual-value> :\n"))
         (pretty-print b1)
         (cat ("<expect-form> :\n"))
         (pretty-print (quote b2))
         (cat ("<expect-value> :\n"))
         (pretty-print b2)
         (orz 'test ("<test-fail-report-end>\n"))))]))

(define (take lis k)
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
        (cons (car lis)
              (recur (cdr lis) (- k 1))))))

(define (drop lis k)
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))

(define (left-of s l)
  (: sexp list -> list)
  (cond [(equal? s (car l)) '()]
        [else (cons (car l) (left-of s (cdr l)))]))

(define (right-of s l)
  (: sexp list -> list)
  (cond [(equal? s (car l)) (cdr l)]
        [else (right-of s (cdr l))]))

(define (sublist l start end)
  (: list index index -> list)
  (cond [(and (eq? 0 start) (<= end 0)) '()]
        [(and (not (eq? 0 start)))
         (sublist (cdr l) (- start 1) (- end 1))]
        [(and (eq? 0 start) (not (eq? 0 end)))
         (cons (car l) (sublist (cdr l) 0 (- end 1)))]))

(define (genlist len)
  (: length -> list)
  (letrec ([recur
            (lambda (len counter)
              (cond [(eq? len counter) '()]
                    [else (cons counter
                                (recur len (+ 1 counter)))]))])
    (recur len 0)))

(define (substitute e p? l)
  (: element (element -> bool) (element ...) -> (element ...))
  (cond [(eq? '() l) '()]
        [(p? (car l)) (cons e (cdr l))]
        [else (cons (car l) (substitute e p? (cdr l)))]))

(define (find-char c s)
  (: char string -> (or curser #f))
  (find-char/curser c s 0))

(define (find-char/curser c s curser)
  (: char string curser -> (or curser #f))
  (if (>= curser (string-length s))
    #f
    (let ([c0 (substring s curser (+ 1 curser))])
      (if (equal? c c0)
        curser
        (find-char/curser c s (+ 1 curser))))))

(define-syntax ->
  (syntax-rules ()
    [(_ a)
     a]
    [(_ a (f1 a1 ...))
     (f1 a a1 ...)]
    [(_ a (f1 a1 ...) b ...)
     (-> (f1 a a1 ...) b ...)]))

(define (pass1/arrow default-level s)
  (: default-level form1/arrow -> form2/arrow)
  (list (pass1/cedent default-level (left-of '-> s))
        (pass1/cedent default-level (right-of '-> s))))

(define (pass1/cedent default-level s)
  (: default-level (form1 ...) -> (form2 ...))
  (match s
    [{} {}]
    [(h . r) (cons (pass1 default-level h)
                   (pass1/cedent default-level r))]))

(define (form1/var? v)
  (and (symbol? v)
       (equal? ":" (substring (symbol->string v) 0 1))))

(define (form1/name? v)
  (and (symbol? v)
       (not (eq? ":" (substring (symbol->string v) 0 1)))))

(define (form1/arrow? v)
  (and (list? v)
       (member '-> v)))

(define (form1/lambda? v)
  (and (list? v)
       (eq? (car v) 'lambda)))

(define (form1/im-bind? v)
  (and (list? v)
       (member ': v)))

(define (form1/ex-bind? v)
  (and (list? v)
       (member '@ v)))

(define (pass1 default-level v)
  (: default-level form1 -> form2)
  (cond [(form1/var? v)
         (list 'form2/var
               (pass1/var default-level v))]
        [(form1/name? v)
         (list 'form2/name
               v)]
        [(form1/arrow? v)
         (list 'form2/arrow
               (pass1/arrow default-level v))]
        [(form1/lambda? v)
         (list 'form2/lambda
               (list (pass1/arrow default-level (cadr v))
                     (map (lambda (x) (pass1/arrow default-level x))
                       (cddr v))))]
        [(form1/im-bind? v)
         (list 'form2/bind
               (list (pass1/cedent 1 (left-of ': v))
                     (pass1/cedent 0 (right-of ': v))
                     #f))]
        [(form1/ex-bind? v)
         (list 'form2/bind
               (list (pass1/cedent 1 (left-of '@ v))
                     (pass1/cedent 0 (right-of '@ v))
                     #t))]
        [else
         (orz 'pass1 ("pass1 can not handle sexp-form:~a" v))]))

(define (pass1/var default-level v)
  (: default-level symbol -> form2/var)
  (let* ([str (symbol->string v)]
         [cursor (find-char "^" str)])
    (if cursor
      (list (string->symbol (substring str 0 cursor))
            (string->number (substring str (+ 1 cursor) (string-length str))))
      (list v default-level))))

(define (pass2/arrow a s)
  (: form2/arrow scope -> (form3/arrow scope))
  (match a
    [{ac sc}
     (match (pass2/cedent ac s)
       [{ac1 s1}
        (match (pass2/cedent sc s1)
          [{sc1 s2}
           {{ac1 sc1} s2}])])]))

(define (pass2/cedent c s)
  (: (form2 ...) scope -> ((form3 ...) scope))
  (match c
    [{} {{} s}]
    [(f . r)
     (match (pass2 f s)
       [{f1 s1}
        (match (pass2/cedent r s1)
          [{c1 s2}
           {(cons f1 c1) s2}])])]))

(define (pass2/lambda l s)
  (: form2/lambda scope -> (form3/lambda scope))
  (match l
    [{a al}
     {{(pass2/arrow a s)
       (map (lambda (x) (pass2/arrow x s))
         al)}
      s}]))

(define (pass2 f s)
  (: form2 scope -> (form2 scope))
  (match f
    [{'form2/var v}
     (match (pass2/var v s)
       [{v1 s1}
        {{'form3/var v1} s1}])]
    [{'form2/name n}
     {{'form3/name n} s}]
    [{'form2/arrow a}
     (match (pass2/arrow a s)
       [{a1 s1}
        {{'form3/arrow a1} s1}])]
    [('form2/lambda l)
     (match (pass2/lambda l s)
       [{l1 s1}
        {{'form3/lambda l1} s1}])]
    [('form2/bind b)
     (match (pass2/bind b s)
       [{b1 s1}
        {{'form3/bind b1} s1}])]))

(define (pass2/var v s)
  (: form2/var scope -> (form3/var scope))
  (match v
    [{symbol level}
     (let ([found (assq symbol s)])
       (if found
         (let ([old (cdr found)])
           {{old level} s})
         (let ([new (vector symbol '())])
           {{new level}
            (cons (cons symbol new) s)})))]))

(define (pass2/bind b s)
  (: form2/bind scope -> (form3/bind scope))
  (match b
    [{vs c leave?}
     (match (pass2/cedent vs s)
       [{vs1 s1}
        (match (pass2/cedent c s1)
          ;; this means vars in vs can occur in c
          [{c1 s2}
           {{vs1 c1 leave?} s2}])])]))

(define (env/pop e)
  (: env -> (data env))
  (match e
    [{(d . r) bs ns}
     {d {r bs ns}}]))

(define (pass3/get-arrow a e)
  (: form3/arrow env -> arrow)
  (match (env/pop (pass3/arrow a e))
    [{{'arrow arrow} __}
     arrow]))

(define (pass3/arrow a e)
  (: form3/arrow env -> env)
  (match e
    [{ds bs ns}
     (match a
       [{ac sc}
        (match (pass3/cedent ac e)
          [{ds1 __ __}
           (match (pass3/cedent sc e)
             [{ds2 __ __}
              {(cons {'arrow {(reverse ds1) (reverse ds2)}}
                     ds)
               bs
               ns}])])])]))

(define (pass3/cedent c e)
  (: (form3 ...) env -> env)
  (match e
    [{ds bs ns}
     (match c
       [{} e]
       [(h . r) (pass3/cedent r (pass3 h e))])]))

(define (pass3/lambda l e)
  (: form3/lambda env -> env)
  (match e
    [{ds bs ns}
     (match l
       [{a al}
        {(cons {'lambda
                   (pass3/get-arrow a e)
                 (map (lambda (x)
                        (pass3/get-arrow x e))
                   al)}
               ds)
         bs
         ns}])]))

(define (pass3 f e)
  (: form3 env -> env)
  (match f
    [{'form3/var x} (pass3/var x e)]
    [{'form3/name x} (pass3/name x e)]
    [{'form3/arrow x} (pass3/arrow x e)]
    [{'form3/lambda x} (pass3/lambda x e)]
    [{'form3/bind x} (pass3/bind x e)]))

(define (pass3/var v e)
  (: form3/var env -> env)
  (match e
    [{ds bs ns}
     ;; actually there is no need to search bs
     ;; but anyway
     {(cons (bs/deep bs (list 'var v)) ds)
      bs
      ns}]))

(define (id->symbol id)
  (vector-ref id 0))

(define (id->ls id)
  (vector-ref id 1))

(define (pass3/name n e)
  (: form3/name env -> env)
  (match e
    [{ds bs ns}
     (let ([found (assq n ns)])
       (if (not found)
         (orz 'pass3/name ("unknow name : ~a~%" n))
         (let ([meaning (cdr found)])
           (match meaning
             [{'cons/type {{ac sc} n1 __}}
              (pass3/name/cons (length ac) n1 e)]
             [{'cons/data {{ac sc} n1 __}}
              (pass3/name/cons (length ac) n1 e)]
             [{'lambda {{ac sc} __}}
              (pass3/name/trunk (length ac) (length sc) {ac sc} n e)]))))]))

(define (pass3/name/cons len name e)
  (: length name env -> env)
  (match e
    [{ds bs ns}
     {(cons {'cons
             ;; dl in cons is as the order of dl in start
             ;; thus no reverse is needed
             {name (sublist ds 0 len)}}
            (sublist ds len (length ds)))
      bs
      ns}]))

(define (pass3/name/trunk len slen a n e)
  (: length length arrow name env -> env)
  (match e
    [{ds bs ns}
     (let* ([a (copy-arrow a)]
            [dl (sublist ds 0 len)]
            ;; dl in trunk is as the order of dl in start
            ;; thus no reverse is needed
            [make-trunk (lambda (i) (list 'trunk (list a n dl i)))])
       {(append (map make-trunk (genlist slen))
                (sublist ds len (length ds)))
        bs
        ns})]))

(define (pass3/bind b e)
  (: form3/bind env -> env)
  (match b
    [{vl c leave?}
     (match (pass3/cedent c e)
       ;; ><><><
       ;; here I assume the c returns only one data
       ;; actual error handling is needed
       [{(d1 . __) __ __}
        (letrec ([recur
                  ;; (: (form3/var ...) env -> env)
                  (lambda (vl e)
                    (match e
                      [{ds bs ns}
                       (match vl
                         [{} e]
                         [({'form3/var {id level}} . r)
                          ;; ><><><
                          ;; no error handling here
                          ;; ><><><
                          ;; need to check if the bind already exist
                          ;; and to check type
                          (id/commit! id {(cons level d1)})
                          (recur r {(if leave?
                                      (cons d1 ds)
                                      ds)
                                    bs
                                    ns})])]))])
          (recur vl e))])]))

(define (id/commit! id ls)
  (: id ls -> id
     [with effect on id])
  (let ()
    (vector-set! id 1 (append ls (vector-ref id 1)))
    id))

(define (bs/find bs v)
  (: bs var -> (or data #f))
  (match v
    [{id level}
     (let* ([level (if (eq? level #f)
                     0
                     level)]
            [found/commit (assq level (id->ls id))])
       (if found/commit
         (cdr found/commit)
         (let* ([found/ls (assq id bs)]
                [found/bind
                 (if found/ls
                   (assq level (cdr found/ls))
                   #f)])
           (if found/bind
             (cdr found/bind)
             #f))))]))

(define (bs/walk bs d)
  (: bs data -> data)
  (match d
    [{'var v}
     (let ([found (bs/find bs v)])
       (if found
         (bs/walk bs found)
         d))]
    [(__ e) d]))

(define (bs/deep bs d)
  (: bs data -> data)
  (letrec* ([bs/deep-list
             (lambda (bs dl)
               (map (lambda (x) (bs/deep bs x)) dl))]
            [bs/deep-arrow
             (lambda (bs a)
               (match a
                 [(dl1 dl2)
                  (list (bs/deep-list bs dl1)
                        (bs/deep-list bs dl2))]))]
            [bs/deep-arrow-list
             (lambda (bs al)
               (map (lambda (a) (bs/deep-arrow bs a)) al))])
    (match (bs/walk bs d)
      [{'var v}
       {'var v}]
      [{'cons (name dl)}
       {'cons {name (bs/deep-list bs dl)}}]
      [{'arrow a} {'arrow (bs/deep-arrow bs a)}]
      [{'lambda (a al)}
       {'lambda {(bs/deep-arrow bs a)
                 (bs/deep-arrow-list bs al)}}]
      [{'trunk (a al dl i)}
       {'trunk
         {(bs/deep-arrow bs a)
          (if (symbol? al)
            al
            (bs/deep-arrow-list bs al))
          (bs/deep-list bs dl)
          i}}])))

(define (copy-arrow a)
  (: arrow -> arrow)
  (match (copy/arrow a '())
    [{a s} a]))

(define (copy/arrow a s)
  (: arrow scope -> (arrow scope))
  (match a
    [{ac sc}
     (match (copy/cedent ac s)
       [{ac1 s1}
        (match (copy/cedent sc s1)
          [{sc1 s2}
           {{ac1 sc1} s2}])])]))

(define (copy/cedent c s)
  (: (data ...) scope -> ((data ...) scope))
  (match c
    [{} {{} s}]
    [(h . r)
     (match (copy h s)
       [{h1 s1}
        (match (copy/cedent r s1)
          [{r1 s2}
           {(cons h1 r1) s2}])])]))

(define (copy/lambda l s)
  (: lambda scope -> (lambda scope))
  (match l
    [{a al}
     (match (copy/arrow a s)
       [{a1 s1}
        (match (copy/arrow-list al s1)
          [{al1 s2}
           {{a1 al1} s2}])])]))

(define (copy/arrow-list al s)
  (: (arrow ...) scope -> ((arrow ...) scope))
  (match al
    [{} {{} s}]
    [(h . r)
     (match (copy/arrow h s)
       [{h1 s1}
        (match (copy/arrow-list r s1)
          [{r1 s2}
           {(cons h1 r1) s2}])])]))

(define (copy d s)
  (: data scope -> (data scope))
  (match d
    [{'var x}
     (match (copy/var x s)
       [{x1 s1}
        {{'var x1} s1}])]
    [{'cons x}
     (match (copy/cons x s)
       [{x1 s1}
        {{'cons x1} s1}])]
    [{'arrow x}
     (match (copy/arrow x s)
       [{x1 s1}
        {{'arrow x1} s1}])]
    [{'lambda x}
     (match (copy/lambda x s)
       [{x1 s1}
        {{'lambda x1} s1}])]
    [{'trunk x}
     (match (copy/trunk x s)
       [{x1 s1}
        {{'trunk x1} s1}])]))

(define (copy/var v s)
  (: var scope -> (var scope))
  (match v
    [{id level}
     (let ([found (assq id s)])
       (if found
         {{(cdr found) level} s}
         (let* ([ls (id->ls id)]
                [id1 (vector (id->symbol id) '())]
                [s1 (cons (cons id id1) s)])
           (match (copy/ls ls s1)
             [{ls1 s2}
              (id/commit! id1 ls1)
              {{id1 level} s2}]))))]))

(define (copy/ls ls s)
  (: ls scope -> (ls scope))
  (match ls
    [{} {{} s}]
    [((level . data) . r)
     (match (copy data s)
       [{data1 s1}
        (match (copy/ls r s1)
          [{r1 s2}
           {(cons (cons level data1)
                  r1)
            s2}])])]))

(define (copy/cons c s)
  (: cons scope -> (cons scope))
  (match c
    [{n dl}
     (match (copy/cedent dl s)
       [{dl1 s1}
        {{n dl1} s1}])]))

(define (copy/trunk p s)
  (: trunk scope -> (trunk scope))
  (match p
    [{a al dl i}
     (if (symbol? al)
       (match (copy/arrow a s)
         [{a1 s1}
          (match (copy/cedent dl s1)
            [{dl1 s2}
             {{a1 al dl1 i} s2}])])
       (match (copy/arrow a s)
         [{a1 s1}
          (match (copy/arrow-list al s1)
            [{al1 s2}
             (match (copy/cedent dl s2)
               [{dl1 s3}
                {{a1 al1 dl1 i} s3}])])]))]))

(define (compute/arrow a e)
  (: arrow env -> report)
  (match e
    [{ds bs ns}
     (match a
       [{ac sc}
        (let ([alen (length ac)]
              [slen (length sc)])
          (match (compute/cedent ac {ds
                                     (cons '(commit-point) bs)
                                     ns})
            [{'fail il} {'fail il}]
            [{'success {ds1 bs1 ns1}}
             (match (unify/data-list
                     (take ds1 alen) (take (drop ds1 alen) alen)
                     {'success
                      {(drop (drop ds1 alen) alen)
                       bs1
                       ns1}})
               [{'fail il} {'fail il}]
               [{'success e2}
                (match (compute/cedent sc e2)
                  [{'fail il} {'fail il}]
                  [{'success {ds3 bs3 ns3}}
                   {'success {ds3 (bs/commit! bs3) ns3}}])])]))])]))

(define (bs/commit! bs)
  (: bs -> bs
     [with effect on part of elements of bs])
  (cond [(equal? '(commit-point) (car bs))
         (cdr bs)]
        [else
         (let* ([pair (car bs)]
                [id (car pair)]
                [ls (cdr pair)])
           (id/commit! id ls)
           (bs/commit! (cdr bs)))]))

(define (compute/cedent c e)
  (: cedent env -> report)
  (match c
    [{} {'success e}]
    [(h . r)
     (match (compute h e)
       [{'fail il} {'fail il}]
       [{'success e1} (compute/cedent r e1)])]))

(define (compute d e)
  (: data env -> report)
  (match e
    [(ds bs ns)
     (match d
       [{'var x} (compute/var x e)]
       [{'cons x} (compute/cons x e)]
       [{'trunk x} (compute/trunk x e)]
       [__ {'success {(cons d ds) bs ns}}])]))

(define (compute/var v e)
  (: var env -> report)
  (match e
    [(ds bs ns)
     (let ([d (bs/deep bs {'var v})])
       (match d
         ;; result found from this var needs to be compute again
         ;; except for fresh var
         [{'var __}
          {'success {(cons d ds) bs ns}}]
         [{__ __}
          (compute d e)]))]))

(define (compute/cons c e)
  (: cons env -> report)
  (match e
    [(ds bs ns)
     (match c
       [(n dl)
        ;; the following reverse
        ;; dl in stack -> dl in function body
        (match (compute/cedent (reverse dl) (list '() bs ns))
          [{'fail il}
           {'fail (cons `(compute/cons
                          fail
                          (cons: ,c))
                        il)}]
          [{'success {ds1 bs1 ns1}}
           {'success {(cons {'cons {n ds1}}
                            ds)
                      bs
                      ns}}])])]))

(define (trunk->trunk* t e)
  (: trunk env -> trunk)
  (match e
    [{ds bs ns}
     (match t
       [{a al dl i}
        (if (not (symbol? al))
          {a al dl i}
          ;; this is the only place (arrow ...) is copied
          (let* ([n al]
                 [found (assq n ns)])
            (if (not found)
              (orz 'trunk->trunk*
                   ("fail~%")
                   ("unknow name : ~a~%" n))
              (let ([meaning (cdr found)])
                (match meaning
                  [{'lambda {{ac sc} al1}}
                   {a (map copy-arrow al1) dl i}]
                  [__
                   (orz 'trunk->trunk*
                        ("trunk->trunk* fail~%" )
                        ("name is not lambda : ~a~%" n))])))))])]))

(define (compute/trunk t e)
  (: trunk env -> report)
  (match e
    [{ds bs ns}
     (match (trunk->trunk* t e)
       [{a al dl i}
        ;; the following reverse
        ;; dl in stack -> dl in function body
        (match (compute/cedent (reverse dl) {{} bs ns})
          [{'fail il}
           {'fail (cons `(compute/trunk
                          fail when computing data-list
                          (data-list: ,dl)
                          (cons: ,c))
                        il)}]
          [{'success e1}
           (match e1
             [{ds1 bs1 ns1}
              (let* ([dl1 ds1]
                     [al1 (filter-arrow-list al dl1 e1)])
                (match al1
                  [{}
                   {'fail {`(compute/trunk
                             no antecedent match
                             (data-list: ,ds1)
                             (arrow-list: ,al)
                             (trunk: ,t))}}]
                  [{a1}
                   (match (compute/arrow a1 e1)
                     ;; after this compute/arrow
                     ;; binds are commited
                     [{'success e2}
                      {'success {(cons (proj i e2) ds)
                                 bs1
                                 ns1}}]
                     [{'fail il} {'fail il}])]
                  [(a1 a2 . __)
                   {'success
                    {(cons {'trunk {a al1 dl1 i}}
                           ds)
                     bs1
                     ns1}}]))])])])]))

(define (filter-arrow-list al dl e)
  (: (arrow ...) (data ...) env -> (arrow ...))
  (if (eq? '() al)
    '()
    (match e
      [{ds bs ns}
       (match (car al)
         [{ac __}
          (let ([alen (length ac)])
            (match (compute/cedent ac e)
              [{'fail __}
               (orz 'filter-arrow-list ("fail to compute/cedent~%"))]
              [{'success {ds1 bs1 ns1}}
               (match (unify/data-list
                       dl (take ds1 alen)
                       {'success {(drop ds1 alen)
                                  bs1
                                  ns1}})
                 [{'fail __}
                  (filter-arrow-list (cdr al) dl e)]
                 [{'success __}
                  (cons (car al)
                        (filter-arrow-list (cdr al) dl e))])]))])])))

(define (proj i e)
  (: index env -> data)
  (match e
    [(ds bs ns)
     (list-ref ds (- (length ds) (+ 1 i)))]))

(define (unify/data-list pl dl r)
  (: (pattern ...) (data ...) report -> report)
  (match r
    [{'fail il} {'fail il}]
    [{'success e}
     (cond [(and (eq? pl '()) (eq? dl '()))
            r]
           [(eq? pl {})
            {'fail {`(unify/data-list
                      fail pl and dl is not of the same length
                      (additional-dl: ,dl))}}]
           [(eq? dl {})
            {'fail {`(unify/data-list
                      fail pl and dl is not of the same length
                      (additional-pl: ,pl))}}]
           [else
            (unify/data-list
             (cdr pl) (cdr dl)
             (unify/data (car pl) (car dl) e))])]))

(define (var/eq? v1 v2)
  (match (list v1 v2)
    [((id1 level1) (id2 level2))
     (and (eq? id1 id2)
          (eq? level1 level2))]))

(define (unify/data p d e)
  (: pattern data env -> report)
  (match e
    [{ds bs ns}
     ;; var -walk-> fresh-var
     (let ([p (bs/walk bs p)]
           [d (bs/walk bs d)])
       (match {p d}
         [{{'var v1} {'var v2}}
          (if (var/eq? v1 v2)
            {'success e}
            {'success {ds
                       (bs/extend bs v1 d)
                       ns}})]
         [{{'var v} __} (unify/var/data v d e)]
         [{__ {'var v}} (unify/var/data v p e)]

         [{{'trunk t1} {'trunk t2}} (unify/trunk t1 t2 e)]
         [{{'trunk t} __} (unify/trunk/data t d e)]
         [{__ {'trunk t}} (unify/trunk/data t p e)]

         [{{'cons c1} {'cons c2}} (unify/cons c1 c2 e)]
         [{{'arrow a1} {'arrow a2}} (unify/arrow a1 a2 e)]
         [{{'lambda l1} {'lambda l2}} (unify/lambda l1 l2 e)]
         [{__ __}
          {'fail {`(unify/data
                    fail to unify
                    (pattern: ,p) (data: ,d))}}]))]))

(define (bs/extend bs v d)
  (: bs var data -> bs)
  (match v
    [{id level}
     (let ([found/ls (assq id bs)])
       (if found/ls
         (substitute (cons id (cons (cons level d)
                                    (cdr found/ls)))
                     (lambda (pair) (eq? (car pair) id))
                     bs)
         (cons (cons id (list (cons level d)))
               bs)))]))

(define (unify/var/data v d e)
  (: var data env -> report)
  (match e
    [{ds bs ns}
     {'success {ds (bs/extend bs v d) ns}}]))

(define (unify/cons c1 c2 e)
  (: cons cons env -> report)
  (match {c1 c2}
    [{{n1 dl1} {n2 dl2}}
     (if (eq? n1 n2)
       (unify/data-list dl1 dl2 {'success e})
       {'fail {`(unify/cons
                 fail
                 (cons1: ,c1)
                 (cons2: ,c2))}})]))

(define (unify/arrow a1 a2 e)
  (: arrow arrow env -> report)
  (match {a1 a2}
    [{{ac1 sc1} {ac2 sc2}}
     (match (unify/data-list ac1 ac2 {'success e})
       [{'success e1}
        (unify/data-list sc1 sc2 {'success e1})]
       [{'fail il}
        {'fail (cons `(unify/arrow
                       fail  (arrow1: ,a1) (arrow2: ,a2))
                     il)}])]))

(define (unify/lambda l1 l2 e)
  (: lambda lambda env -> report)
  (match {l1 l2}
    [{{a1 al1} {a2 al2}}
     (unify/arrow-list al1 al2 (unify/arrow a1 a2 e))]))

(define (unify/arrow-list al1 al2 r)
  (: (arrow ...) (arrow ...) report -> report)
  (match r
    [{'fail il} {'fail il}]
    [{'success e}
     (if (eq? al1 {})
       r
       (unify/arrow-list
        (cdr al1) (cdr al2)
        (unify/arrow (car al1) (car al2) e)))]))

(define (unify/trunk t1 t2 e)
  (: trunk trunk env -> report)
  (match {(trunk->trunk* t1 e) (trunk->trunk* t2 e)}
    [{{a1 al1 dl1 i1} {a2 al2 dl2 i2}}

     ;; (if (eq? i1 i2)
     ;;   (unify/data-list dl1 dl2 (unify/lambda {a1 al1} {a2 al2} e))
     ;;   {'fail {`(unify/trunk
     ;;             fail indexes are different
     ;;             (trunk1: ,t1)
     ;;             (trunk2: ,t2))}})
     ;; ;; the above will diverge
     ;; ;; while
     ;; ;; the following make it impossible
     ;; ;; to unify the arrow-list of trunk

     (if (equal? {a1 al1 i1} {a2 al2 i2})
       (unify/data-list dl1 dl2 {'success e})
       {'fail {`(unify/trunk
                 fail
                 (trunk1: ,t1)
                 (trunk2: ,t2))}})]))

(define (unify/trunk/data t d e)
  (: trunk data env -> report)
  (match (compute/trunk t e)
    [{'fail il}
     {'fail (cons `(unify/trunk/data
                    (trunk: ,t)
                    (data: ,d))
                  il)}]
    [{'success e1}
     (match (env/pop e1)
       [{{'trunk t1} e2}
        {'fail {`(unify/trunk/data
                  (trunk: ,t)
                  compute to
                  (trunk: ,t1))}}]
       [{d1 e2}
        (unify/data d1 d e2)])]))

(define check? #t)
(define (check+) (set! check? #t) #t)
(define (check-) (set! check? #f) #f)

(define-syntax eva
  (syntax-rules ()
    [(eva e ...)
     (eva/top-list
      (map parse/top (quote (e ...)))
      '(()
        ()
        ((type . (cons/type ((()
                              (cons (type ())))
                             type
                             type))))))]))

(define (eva/top-list tl e)
  (: (top ...) env -> env)
  (match tl
    [{} e]
    [(t . r) (eva/top-list r (eva/top t e))]))

(define (parse/top s)
  (: sexp-top -> top)
  (match s
    [('dt n a . body)
     {'dt {{n a} (parse/top/dt-body body)}}]
    [('df n a . al)
     {'df {{n a} al}}]
    [{'ap a}
     {'ap a}]))

(define (parse/top/dt-body body)
  (: dt-body -> ((form1/name form1/arrow) ...))
  (cond [(eq? '() body) '()]
        [(eq? '() (cdr body))
         (orz 'parse/top/dt-body ("wrong body : ~a~%" body))]
        [else
         (cons (list (car body) (cadr body))
               (parse/top/dt-body (cddr body)))]))

(define (eva/top t e)
  (: top env -> env)
  (match t
    [{'dt dt} (eva/dt dt e)]
    [{'df df} (eva/df df e)]
    [{'ap a} (eva/ap a e)]))

(define (form1/arrow->arrow a e)
  (: form1/arrow env -> arrow)
  (match (pass2/arrow (pass1/arrow 0 a) {})
    [{a1 s} (pass3/get-arrow a1 e)]))

(define (eva/dt dt e)
  (: ((form1/name form1/arrow) ((form1/name form1/arrow) ...)) env -> env)
  (match e
    [{ds bs ns}
     (match dt
       [{{n a} nal}
        (let* ([nl (map car nal)]
               [a0 (form1/arrow->arrow a e)]
               [ns1 (cons (cons n
                                {'cons/type {a0 n nl}})
                          ns)])
          (eva/dt/data-constructor-list n nal {ds bs ns1}))])]))

(define (eva/dt/data-constructor type-name na e)
  (: name (form1/name form1/arrow) env -> env)
  (match e
    [{ds bs ns}
     (match na
       [{n a}
        (let ([a0 (form1/arrow->arrow a e)])
          {ds
           bs
           (cons (cons n {'cons/data {a0 n type-name}})
                 ns)})])]))

(define (eva/dt/data-constructor-list type-name nal e)
  (: name ((form1/name form1/arrow) ...) env -> env)
  (match nal
    [{} e]
    [(na . r)
     (eva/dt/data-constructor-list
      type-name r
      (eva/dt/data-constructor type-name na e))]))

(define (eva/df df e)
  (: ((form1/name form1/arrow) (form1/arrow ...)) env -> env)
  (match e
    [{ds bs ns}
     (match df
       [{{n a} al}
        (let* ([a0 (form1/arrow->arrow a e)]
               ;; need to put the type into ns first
               ;; for recursive call in arrow-list
               ;; that is
               ;; in ns
               ;; type global-bindings and arrow-list global-bindings
               ;; must be separately interfaced
               [ns0 (cons (cons n {'lambda {a0 'placeholder}})
                          ns)]
               [al0 (map (lambda (x)
                           (form1/arrow->arrow x {ds bs ns0}))
                      al)]
               [ns1 (cons (cons n
                                {'lambda {a0 al0}})
                          ns)])
          (if (not check?)
            {ds bs ns1}
            (match (check (copy-arrow a0) (map copy-arrow al0)
                          {ds bs ns1})
              ;; note that the bs of the env
              ;; returned by check is not clean
              ;; thus e1 is not used as return env
              [{'success e1} {ds bs ns1}]
              [{'fail il}
               (cat ("eva/df fail to define : ~a~%" df))
               (pretty-print il)
               (orz 'eva/df ("end of report~%"))])))])]))

(define (eva/ap a e)
  (: form1/arrow env -> env)
  (let ([a0 (form1/arrow->arrow a e)])
    (match (compute/arrow a0 e)
      [{'success e1} e1]
      [{'fail il}
       (cat ("eva/ap fail~%"))
       (pretty-print il)
       (cat ("~%"))
       (orz 'eva/ap ("end of report~%"))])))

(define (check t al e)
  (: arrow (arrow ...) env -> report)
  (match al
    [{} {'success e}]
    [(a . r)
     (match (check/arrow t a e)
       [{'success e1}
        ;; note that the above return env is droped
        ;; this is viewed as undo
        (check t r e)]
       [{'fail il} {'fail il}])]))

(define (check/arrow t a e)
  (: arrow arrow env -> report)
  (match (list t a)
    [{{tac tsc} {ac sc}}
     (let ([alen (length ac)]
           [talen (length tac)]
           [slen (length sc)]
           [tslen (length tsc)])
       (match (compute/cedent tac e)
         [{'fail il}
          {'fail (cons `(check/arrow
                         fail on compute/cedent
                         (type-antecedent: ,tac))
                       il)}]
         [{'success e1}
          (match (type-compute/cedent ac e1)
            [{'fail il}
             {'fail (cons `(check/arrow
                            fail on compute/cedent
                            (antecedent: ,ac))
                          il)}]
            [{'success e2}
             (match e2
               [{ds2 bs2 ns2}
                (match (unify/data-list
                        (take ds2 talen)
                        (take (drop ds2 talen) alen)
                        {'success {(drop (drop ds2 talen) alen)
                                   bs2
                                   ns2}})
                  [{'fail il}
                   {'fail (cons `(check/arrow
                                  fail on unify/data-list
                                  (type-antecedent: ,tac)
                                  (antecedent: ,ac))
                                il)}]
                  [{'success e3}
                   (match (compute/cedent tsc e3)
                     [{'fail il}
                      {'fail (cons `(check/arrow
                                     fail on compute/cedent
                                     (type-succedent: ,tsc))
                                   il)}]
                     [{'success e4}
                      (match (type-compute/cedent sc e4)
                        [{'fail il}
                         {'fail (cons `(check/arrow
                                        fail on
                                        (succedent: ,sc))
                                      il)}]
                        [{'success e5}
                         (match e5
                           [(ds5 bs5 ns5)
                            (unify/data-list
                             (take ds5 tslen)
                             (take (drop ds5 tslen) slen)
                             {'success {(drop (drop ds5 tslen) slen)
                                        bs5
                                        ns5}})])])])])])])]))]))

(define (type-compute/cedent c e)
  (: (data ...) env -> report)
  (match c
    [{} {'success e}]
    [(d . r)
     (match (type-compute d e)
       [{'fail il} {'fail il}]
       [{'success e1}
        (type-compute/cedent r e1)])]))

(define (type-compute d e)
  (: data env -> report)
  (match d
    [{'var x} (type-compute/var x e)]
    [{'cons x} (type-compute/cons x e)]
    [{'arrow x} (type-compute/arrow x e)]
    [{'lambda x} (type-compute/lambda x e)]
    [{'trunk x} (type-compute/trunk x e)]))

(define (type-compute/var v e)
  (: var env -> report)
  (match v
    [{id level}
     (compute/var {id (+ 1 level)} e)]))

(define (type-compute/cons c e)
  (: cons env -> report)
  (match e
    [{ds bs ns}
     (match c
       [{n dl}
        (let ([found (assq n ns)])
          (if (not found)
            (orz 'type-compute/cons
                 ("unknow name : ~a~%" n)
                 ("cons : ~a~%" c))
            (let ([meaning (cdr found)])
              (match meaning
                [{any-type (t . __)}
                 (match (type-compute/cedent (reverse dl) e)
                   [{'fail il} {'fail il}]
                   [{'success e1}
                    (compute/arrow (copy-arrow t) e1)])]))))])]))

(define (type-compute/arrow a e)
  (: arrow env -> report)
  (orz 'type-compute/arrow
       ("arrow is not handled for now~%")))

(define (type-compute/lambda l e)
  (: lambda env -> report)
  (match e
    [{ds bs ns}
     (match l
       [{a al}
        {'success {(cons {'arrow a} ds)
                   bs
                   ns}}])]))

(define (type-compute/trunk t e)
  (: trunk env -> report)
  (match e
    [{ds bs ns}
     (match t
       [{a __ dl i}
        (match (type-compute/cedent (reverse dl) {{} bs ns})
          [{'fail il} {'fail il}]
          [{'success e1}
           (match e1
             [{ds1 bs1 ns1}
              (match (compute/arrow (copy-arrow a) e1)
                [{'fail il} {'fail il}]
                [{'success e2}
                 {'success {(cons (proj i e2) ds)
                            bs1
                            ns1}}])])])])]))
