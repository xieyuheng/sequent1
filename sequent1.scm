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

(define (pass2/get-arrow a s)
  (: form2/arrow scope -> form3/arrow)
  (match (pass2/arrow a s)
    [{a1 s} a1]))

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
     {{(pass2/get-arrow a s)
       (map (lambda (x) (pass2/get-arrow x s))
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
    [{'form2/lambda l}
     (match (pass2/lambda l s)
       [{l1 s1}
        {{'form3/lambda l1} s1}])]
    [{'form2/bind b}
     (match (pass2/bind b s)
       [{b1 s1}
        {{'form3/bind b1} s1}])]))

(define id/counter 0)

(define (id/new n ls)
  (: name ls -> id)
  (set! id/counter (+ 1 id/counter))
  (vector (cons n id/counter) ls))

(define (pass2/var v s)
  (: form2/var scope -> (form3/var scope))
  (match v
    [{symbol level}
     (let ([found (assq symbol s)])
       (if found
         (let ([old (cdr found)])
           {{old level} s})
         (let ([new (id/new symbol '())])
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
        (match (pass3/cedent ac {{} bs ns})
          [{dl-ac __ __}
           (match (pass3/cedent sc {{} bs ns})
             [{dl-sc __ __}
              {(cons {'arrow {(reverse dl-ac) (reverse dl-sc)}}
                     ds)
               bs
               ns}])])])]))

(define (pass3/get-arrow-check ta a e)
  (: arrow form3/arrow env -> arrow)
  (match (env/pop (pass3/arrow-check ta a e))
    [{{'arrow arrow} __}
     arrow]))

(define (pass3/arrow-check ta a e)
  (: arrow form3/arrow env -> env)
  (let ([ta (copy-arrow ta)])
    (match e
      [{ds bs ns}
       (match {ta a}
         [{{tac tsc} {ac sc}}
          (match (pass3/cedent ac {{} (cons '(commit-point) bs) ns})
            [{dl-ac bs-ac __}
             (match (type-compute/cedent (reverse dl-ac) {{} bs-ac ns})
               [{'fail il}
                (orz 'pass3/arrow-check
                  ("fail to type-compute/cedent~%")
                  ("ac : ~a~%" (reverse dl-ac))
                  ("info-list : ~a~%" il))]
               [{'success {type-dl-ac type-bs-ac __}}
                (match (compute/cedent tac {{} type-bs-ac ns})
                  [{'fail il}
                   (orz 'pass3/arrow-check
                     ("fail to compute/cedent~%")
                     ("tac : ~a~%" tac)
                     ("info-list : ~a~%" il))]
                  [{'success {dl-tac bs-tac __}}
                   (match (unify/data-list
                           dl-tac type-dl-ac
                           {'success {{} bs-tac ns}})
                     [{'fail il}
                      (orz 'pass3/arrow-check
                        ("fail to unify/data-list~%")
                        ("dl-tac : ~a~%" dl-tac)
                        ("type-dl-ac : ~a~%" type-dl-ac)
                        ("info-list : ~a~%" il))]
                     [{'success {__ bs-antecedent __}}
                      (bs/commit! bs-antecedent)
                      (match (pass3/cedent sc {{} (cons '(commit-point) bs) ns})
                        [{dl-sc bs-sc __}
                         (match (type-compute/cedent (reverse dl-sc) {{} bs-sc ns})
                           [{'fail il}
                            (orz 'pass3/arrow-check
                              ("fail to type-compute/cedent~%")
                              ("sc : ~a~%" (reverse dl-sc))
                              ("info-list : ~a~%" il))]
                           [{'success {type-dl-sc type-bs-sc __}}
                            (match (compute/cedent tsc {{} type-bs-sc ns})
                              [{'fail il}
                               (orz 'pass3/arrow-check
                                 ("fail to compute/cedent~%")
                                 ("tsc : ~a~%" tsc)
                                 ("info-list : ~a~%" il))]
                              [{'success {dl-tsc bs-tsc __}}
                               (match (unify/data-list
                                       dl-tsc type-dl-sc
                                       {'success {{} bs-tsc ns}})
                                 [{'fail il}
                                  (orz 'pass3/arrow-check
                                    ("fail to unify/data-list:~%")
                                    ("dl-tsc : ~a~%" dl-tsc)
                                    ("type-dl-sc : ~a~%" type-dl-sc)
                                    ("info-list : ~a~%" il))]
                                 [{'success {__ bs-succedent __}}
                                  (bs/commit! bs-succedent)
                                  {(cons {'arrow {(reverse dl-ac) (reverse dl-sc)}}
                                         ds)
                                   bs
                                   ns}])])])])])])])])])])))

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
        (let ([ta (pass3/get-arrow a e)])
          {(cons {'lambda
                     {ta
                      (map (lambda (x)
                             (pass3/get-arrow-check ta x e))
                        al)}}
                 ds)
           bs
           ns})])]))

(define (pass3 f e)
  (: form3 env -> env)
  (match f
    [{'form3/var x} (pass3/var x e)]
    [{'form3/name 'apply} (pass3/apply e)]
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
     {(cons (bs/deep bs {'var v}) ds)
      bs
      ns}]))

(define (pass3/apply e)
  (: env -> env)
  (match e
    [{(d . r) bs ns}
     (pass3/apply/data d {r bs ns})]))

(define (pass3/apply/data d e)
  (: data env -> env)
  (match d
    [{'arrow x}
     (pass3/apply/arrow x e)]
    [{'lambda x}
     (pass3/apply/lambda x e)]
    [{'var x}
     (pass3/apply/var x e)]
    [__
     (orz 'pass3/apply/data
       ("can only apply arrow or lambda or var~%")
       ("but the data at the top of data-stack is : ~a~%" d))]))

(define (pass3/apply/arrow a e)
  (: arrow env -> env)
  (match e
    [{ds bs ns}
     (let* ([t (infer/arrow a e)])
       (match t
         [{ac sc}
          (let* ([alen (length ac)]
                 [slen (length sc)]
                 [dl (sublist ds 0 alen)]
                 [make-trunk
                  (lambda (i)
                    {'trunk
                      {t {'tody/arrow-list {a}} dl i}})])
            {(append (reverse (map make-trunk (genlist slen)))
                     (sublist ds alen (length ds)))
             bs
             ns})]))]))

(define (pass3/apply/lambda l e)
  (: lambda env -> env)
  (match e
    [{ds bs ns}
     (match l
       [{{ac sc} al}
        (let* ([alen (length ac)]
               [slen (length sc)]
               [dl (sublist ds 0 alen)]
               [make-trunk
                (lambda (i)
                  {'trunk
                    {{ac sc} {'tody/arrow-list al} dl i}})])
          {(append (reverse (map make-trunk (genlist slen)))
                   (sublist ds alen (length ds)))
           bs
           ns})])]))

(define (pass3/apply/var v e)
  (: var env -> env)
  (match e
    [{ds bs ns}
     (if (not (var/fresh? v e))
       (pass3/apply/data (bs/deep bs {'var v}) e)
       (match (type-compute/var v e)
         [{'fail il}
          (orz 'pass3/apply/var
            ("fail to compute the type of var : ~a~%" v)
            ("report info :~%~a~%" il))]
         [{'success {(d . __) __ __}}
          (match d
            [{'arrow {ac sc}}
             (let* ([alen (length ac)]
                    [slen (length sc)]
                    [dl (sublist ds 0 alen)]
                    [make-trunk
                     (lambda (i)
                       {'trunk
                         {{ac sc} {'tody/var v} dl i}})])
               {(append (reverse (map make-trunk (genlist slen)))
                        (sublist ds alen (length ds)))
                bs
                ns})]
            [__
             (orz 'pass3/apply/var
               ("to form trunk from var~%")
               ("the type of var must be a arrow~%")
               ("var : ~a~%" v)
               ("type of var : ~a~%" d))])]))]))

(define (id->name id)
  (car (vector-ref id 0)))

(define (id->counter id)
  (cdr (vector-ref id 0)))

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

(define (pass3/name/trunk alen slen a n e)
  (: length length arrow name env -> env)
  (match e
    [{ds bs ns}
     (let* ([a (copy-arrow a)]
            [dl (sublist ds 0 alen)]
            ;; dl in trunk is as the order of dl in stack
            ;; thus no reverse is needed
            [make-trunk (lambda (i) {'trunk {a {'tody/name n} dl i}})])
       {(append (reverse (map make-trunk (genlist slen)))
                (sublist ds alen (length ds)))
        bs
        ns})]))

(define (pass3/bind b e)
  (: form3/bind env -> env)
  (match e
    [{ds bs ns}
     (match b
       [{vl c leave?}
        (match (pass3/cedent c {{} bs ns})
          [{ds1 __ __}
           (if (not (eq? 1 (length ds1)))
             (orz 'pass3/bind
               ("the cedent in bind should only return one data~%")
               ("bind : ~a~%" b))
             (let ([d1 (car ds1)])
               (letrec ([recur
                         (lambda (vl e)
                           (: (form3/var ...) env -> env)
                           (match e
                             [{ds bs ns}
                              (match vl
                                [{} e]
                                [({'form3/var {id level}} . r)
                                 (if (not (var/fresh? {id level} e))
                                   (orz 'pass3/bind
                                     ("var is not fresh : ~a~%" {id level})
                                     ("env : ~a~%" e))
                                   (if (not
                                        (match (consistent-check
                                                {id level} d1 e)
                                          [{'fail __} #f]
                                          [{'success __} #t]))
                                     (orz 'pass3/bind
                                       ("var data is not consistent~%")
                                       ("var : ~a~%" {id level})
                                       ("data : ~a~%" d1))
                                     (let ()
                                       (id/commit! id {(cons level d1)})
                                       (recur r {(if leave?
                                                   (cons d1 ds)
                                                   ds)
                                                 bs
                                                 ns}))))])]))])
                 (recur vl e))))])])]))

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
    [{__ e} d]))

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
      ;; a var is fresh after bs/walk
      [{'var v}
       {'var v}]
      [{'cons {name dl}}
       {'cons {name (bs/deep-list bs dl)}}]
      [{'arrow a} {'arrow (bs/deep-arrow bs a)}]
      [{'lambda {a al}}
       {'lambda {(bs/deep-arrow bs a)
                 (bs/deep-arrow-list bs al)}}]
      [{'trunk {a tody dl i}}
       {'trunk
         {(bs/deep-arrow bs a)
          (match tody
            [{'tody/var v}
             (match (bs/deep bs {'var v})
               [{'var v1} {'tody/var v1}]
               [{'arrow a1} {'tody/arrow-list {a1}}]
               [{'lambda {a al}} {'tody/arrow-list al}]
               [d
                (orz 'bs/deep
                  ("find something wrong from the var in the tody of trunk~%")
                  ("data : ~a~%" d))])]
            [{'tody/name n}
             {'tody/name n}]
            [{'tody/arrow-list al}
             {'tody/arrow-list (bs/deep-arrow-list bs al)}])
          (bs/deep-list bs dl)
          i}}])))

(define (var/fresh? v e)
  (: var env -> bool)
  (match e
    [{ds bs ns}
     (equal? (bs/walk bs {'var v})
             {'var v})]))

(define (copy-arrow a)
  (: arrow -> arrow)
  (match (copy/arrow a '())
    [{a1 __} a1]))

(define (copy-cedent c)
  (: cedent -> cedent)
  (match (copy/cedent c '())
    [{c1 __} c1]))

(define (copy/arrow a s)
  (: arrow scope -> (arrow scope))
  (match a
    [{ac sc}
     (match (copy/cedent ac s)
       [{ac1 s1}
        (match (copy/cedent sc s1)
          [{sc1 s2}
           {{ac1 sc1} s2}])])]))

(define (copy/data-list dl s)
  (: (data ...) scope -> ((data ...) scope))
  (copy/cedent dl s))

(define (copy/cedent c s)
  (: cedent scope -> (cedent scope))
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
                [id1 (id/new (id->name id) '())]
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
     (match (copy/data-list dl s)
       [{dl1 s1}
        {{n dl1} s1}])]))

(define (copy/trunk p s)
  (: trunk scope -> (trunk scope))
  (match p
    [{a tody dl i}
     (match tody
       [{'tody/var v}
        (match (copy/arrow a s)
          [{a1 s1}
           (match (copy/data-list dl s1)
             [{dl1 s2}
              (match (copy/var v s2)
                [{v1 s3}
                 {{a1 {'tody/var v1} dl1 i} s3}])])])]
       [{'tody/name n}
        (match (copy/arrow a s)
          [{a1 s1}
           (match (copy/data-list dl s1)
             [{dl1 s2}
              {{a1 {'tody/name n} dl1 i} s2}])])]
       [{'tody/arrow-list al}
        (match (copy/arrow a s)
          [{a1 s1}
           (match (copy/arrow-list al s1)
             [{al1 s2}
              (match (copy/data-list dl s2)
                [{dl1 s3}
                 {{a1 {'tody/arrow-list al1} dl1 i} s3}])])])])]))

(define (compute/arrow a e)
  (: arrow env -> report)
  (match e
    [{ds bs ns}
     (match a
       [{ac sc}
        (let ([alen (length ac)]
              [slen (length sc)])
          (match (compute/cedent ac {ds (cons '(commit-point) bs) ns})
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
    ;; proper tail call
    [{h} (compute h e)]
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
       ;; note that arrow in arrow is computed as literal
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

(define (compute/trunk t e)
  (: trunk env -> report)
  (match t
    [{a tody dl i}
     (match tody
       [{'tody/var __} (compute/trunk/tody/var t e)]
       [{'tody/name __} (compute/trunk/tody/name t e)]
       [{'tody/arrow-list __} (compute/trunk/tody/arrow-list t e)])]))

(define (compute/trunk/tody/var t e)
  (: trunk env -> report)
  (match e
    [{ds bs ns}
     (match t
       [{a {'tody/var v} dl i}
        (match (bs/deep bs {'var v})
          [{'var v1}
           {'success
            {(cons {'trunk {a {'tody/var v1} dl i}} ds)
             bs
             ns}}]
          [{'arrow a1}
           (compute/trunk/tody/arrow-list
            {a {'tody/arrow-list {a1}} dl i} e)]
          [{'lambda {a1 al}}
           (compute/trunk/tody/arrow-list
            ;; I can use a1 or a
            ;; I use a here
            {a {'tody/arrow-list al} dl i} e)]
          [d
           (orz 'compute/trunk/tody/var
             ("find something wrong from the var in the tody of trunk~%")
             ("data : ~a~%" d))])])]))

(define (compute/trunk/tody/name t e)
  (: trunk env -> report)
  (match e
    [{ds bs ns}
     (match t
       [{a {'tody/name n} dl i}
        (compute/trunk/tody/arrow-list (trunk->trunk* t e) e)])]))

(define (compute/trunk/tody/arrow-list t e)
  (: trunk env -> report)
  (match e
    [{ds bs ns}
     (match t
       [{a {'tody/arrow-list al} dl i}
        ;; the following reverse
        ;; dl in stack -> dl in function body
        (match (compute/cedent (reverse dl) {{} bs ns})
          [{'fail il}
           {'fail (cons `(compute/trunk/tody/arrow-list
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
                   {'fail {`(compute/trunk/tody/arrow-list
                             no antecedent match
                             (data-list: ,ds1)
                             (arrow-list: ,al)
                             (trunk: ,t))
                           `(list-unify/antecedent
                             report
                             ,(list-unify/antecedent al dl1 e1))}}]
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
                    {(cons {'trunk {a {'tody/arrow-list al1} dl1 i}}
                           ds)
                     bs1
                     ns1}}]))])])])]))

(define (trunk->trunk* t e)
  (: trunk env -> trunk)
  (match e
    [{ds bs ns}
     (match t
       [{a {'tody/name n} dl i}
        (let ([found (assq n ns)])
          (if (not found)
            (orz 'trunk->trunk*
              ("fail~%")
              ("unknow name : ~a~%" n))
            (let ([meaning (cdr found)])
              (match meaning
                [{'lambda {{ac sc} al1}}
                 {a {'tody/arrow-list (map copy-arrow al1)} dl i}]
                [__
                 (orz 'trunk->trunk*
                   ("trunk->trunk* fail~%" )
                   ("name is not lambda : ~a~%" n))]))))]
       [{a tody dl i} {a tody dl i}])]))

(define (list-unify/antecedent al dl e)
  (: (arrow ...) (data ...) env -> ((report arrow) ...))
  (map (lambda (a)
         (match e
           [{ds bs ns}
            (match a
              [{ac __}
               {a (match (compute/cedent ac {{} bs ns})
                    [{'fail il} {'fail (cons `(list-unify/antecedent
                                               fail to compute/cedent
                                               (ac: ,ac))
                                             il)}]
                    [{'success {ds1 bs1 ns1}}
                     (unify/data-list
                      dl ds1
                      {'success {ds bs1 ns1}})])}])]))
    al))

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

(define (print/cedent c e)
  (: cedent env -> [effect on terminal])
  (match c
    [{} (void)]
    [{d} (print/data d e)]
    [(d . r)
     (print/data d e)
     (format #t " ")
     (print/cedent r e)]))

(define (print/data-list dl e)
  (: (data ...) env -> [effect on terminal])
  (print/cedent (reverse dl) e))

(define (print/data d e)
  (: data env -> [effect on terminal])
  (match d
    [{'var x} (print/var x e)]
    [{'cons x} (print/cons x e)]
    [{'arrow x} (print/arrow x e)]
    [{'lambda x} (print/lambda x e)]
    [{'trunk x} (print/trunk x e)]))

(define (print/var v e)
  (: var env -> [effect on terminal])
  (match v
    [{id level}
     (let ([name (id->name id)]
           [counter (id->counter id)])
       (format #t ":~a:~a^~a" counter name level))]))

(define (print/cons c e)
  (: cons env -> [effect on terminal])
  (match c
    [{n dl}
     (format #t "[")
     (print/data-list dl e)
     (if (null? dl)
       (format #t "~a]" n)
       (format #t " ~a]" n))]))

(define (print/arrow a e)
  (: arrow env -> [effect on terminal])
  (match a
    [{ac sc}
     (format #t "(")
     (print/cedent ac e)
     (format #t " -> ")
     (print/cedent sc e)
     (format #t ")")]))

(define (print/lambda l e)
  (: lambda env -> [effect on terminal])
  (match l
    [{a al}
     (format #t "<lambda>")]))

(define (print/trunk t e)
  (: trunk env -> [effect on terminal])
  (match t
    [{a tody dl i}
     (format #t "<trunk>")]))

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
    [{{id1 level1} {id2 level2}}
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
            (unify/var/data v1 d e))]
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
  (: fresh-var data env -> report)
  (match e
    [{ds bs ns}
     (match (consistent-check v d e)
       [{'fail il}
        {'fail (cons `(unify/var/data
                       consistent-check fail
                       (v: ,v)
                       (d: ,d))
                     il)}]
       [{'success __}
        {'success {ds (bs/extend bs v d) ns}}])]))

(define (consistent-check v d e)
  (: fresh-var data env -> report)
  (match {v e}
    [{{id level} {ds bs ns}}
     (match {(var/highest? v e) (var/lowest? v e)}
       [{#t #t} {'success e}]
       [{#t #f}
        (match (var/below v e)
          [{{__ low-level} low-d}
           (consistent-check/level-diff (- level low-level) low-d d e)])]
       [{#f #t}
        (match (var/above v e)
          [{{__ high-level} high-d}
           (consistent-check/level-diff (- high-level level) d high-d e)])]
       [{#f #f}
        (match (var/below v e)
          [{{__ low-level} low-d}
           (match (consistent-check/level-diff (- level low-level) low-d d e)
             [{'fail il} {'fail il}]
             [{'success __}
              (match (var/above v e)
                [{{__ high-level} high-d}
                 (consistent-check/level-diff (- high-level level) d high-d e)])])])])]))

(define (consistent-check/level-diff level-diff d1 d2 e)
  (: level-diff data data env -> report)
  (match e
    [{ds bs ns}
     (match (type-compute/repeat level-diff d1 e)
       [{'fail il} {'fail il}]
       [{'success {(d0 . __) bs1 ns1}}
        (unify/data d0 d2 {ds bs1 ns1})])]))

(define (type-compute/repeat c d e)
  (: counter data env -> report)
  (match e
    [{ds bs ns}
     (match (eq? 0 c)
       [#t {'success {(cons d ds) bs ns}}]
       [#f (match (type-compute d e)
             [{'fail il} {'fail il}]
             [{'success {(d1 . r) bs1 ns1}}
              (type-compute/repeat (- c 1) d1 {r bs1 ns1})])])]))

(define (var/highest? v e)
  (: fresh-var env -> bool)
  (match e
    [{ds bs ns}
     (match v
       [{id level}
        (let* ([found (assq id bs)]
               [ls (append (id->ls id)
                           (if found (cdr found) '()))])
          (list-every?
           (lambda (x) (> level (car x)))
           ls))])]))

(define (var/lowest? v e)
  (: fresh-var env -> bool)
  (match e
    [{ds bs ns}
     (match v
       [{id level}
        (let* ([found (assq id bs)]
               [ls (append (id->ls id)
                           (if found (cdr found) '()))])
          (list-every?
           (lambda (x) (< level (car x)))
           ls))])]))

(define (var/above v e)
  (: fresh-var env -> (var data))
  (match e
    [{ds bs ns}
     (match v
       [{id level}
        (let* ([found (assq id bs)]
               [ls (append (id->ls id)
                           (if found (cdr found) '()))])
          (let ([pair
                 (car (filter (lambda (x) (> (car x) level))
                              (sort (lambda (x y) (< (car x) (car y)))
                                    ls)))])
            {{id (car pair)} (cdr pair)}))])]))

(define (var/below v e)
  (: fresh-var env -> (var data))
  (match e
    [{ds bs ns}
     (match v
       [{id level}
        (let* ([found (assq id bs)]
               [ls (append (id->ls id)
                           (if found (cdr found) '()))])
          (let ([pair
                 (car (filter (lambda (x) (< (car x) level))
                              (sort (lambda (x y) (> (car x) (car y)))
                                    ls)))])
            {{id (car pair)} (cdr pair)}))])]))

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
  (match (unify/trunk/syntactic t1 t2 e)
    [{'success e1} {'success e1}]
    [{'fail il1}
     (match (unify/trunk/semantic t1 t2 e)
       [{'success e2} {'success e2}]
       [{'fail il2}
        {'fail (append il2 il1)}])]))

(define (unify/trunk/syntactic t1 t2 e)
  (: trunk trunk env -> report)
  (match {t1 t2}
    [{{a1 tody1 dl1 i1} {a2 tody2 dl2 i2}}
     (if (not (eq? i1 i2))
       {'fail {`(unify/trunk/syntactic
                 fail indexes are different
                 (trunk1: ,t1)
                 (trunk2: ,t2))}}
       (match {tody1 tody2}
         ;; about name
         [{{'tody/name n1} {'tody/name n2}}
          (if (eq? n1 n2)
            (unify/data-list dl1 dl2 (unify/arrow a1 a2 e))
            {'fail {`(unify/trunk/syntactic
                      fail names are different
                      (trunk1: ,t1)
                      (trunk2: ,t2))}})]
         [{{'tody/name n} {'tody/var v}}
          (unify/trunk/syntactic (trunk->trunk* t1 e) t2 e)]
         [{{'tody/var v} {'tody/name n}}
          (unify/trunk/syntactic  t1 (trunk->trunk* t2 e) e)]
         [{{'tody/name n} {'tody/arrow-list al}}
          (unify/trunk/syntactic (trunk->trunk* t1 e) t2 e)]
         [{{'tody/arrow-list al} {'tody/name n}}
          (unify/trunk/syntactic  t1 (trunk->trunk* t2 e) e)]
         ;; about var
         [{{'tody/var v1} {'tody/var v2}}
          (match (unify/data {'var v1} {'var v2} e)
            [{'fail il} {'fail il}]
            [{'success e1}
             (unify/data-list dl1 dl2 (unify/arrow a1 a2 e1))])]
         [{{'tody/var v} {'tody/arrow-list al}}
          (match (unify/data {'var v} {'lambda {a2 al}} e)
            [{'fail il} {'fail il}]
            [{'success e1}
             (unify/data-list dl1 dl2 (unify/arrow a1 a2 e1))])]
         [{{'tody/arrow-list al} {'tody/var v}}
          (match (unify/data {'lambda {a1 al}} {'var v} e)
            [{'fail il} {'fail il}]
            [{'success e1}
             (unify/data-list dl1 dl2 (unify/arrow a1 a2 e1))])]
         ;; about arrow-list
         [{{'tody/arrow-list al1} {'tody/arrow-list al2}}
          (unify/data-list
           dl1 dl2
           (unify/lambda {a1 al1} {a2 al2} e))]))]))

(define (unify/trunk/semantic t1 t2 e)
  (: trunk trunk env -> report)
  (match {t1 t2}
    [{{a1 tody1 dl1 i1} {a2 tody2 dl2 i2}}
     (match {tody1 tody2}
       ;; about name
       [{{'tody/name n} __}
        (unify/trunk/semantic (trunk->trunk* t1 e) t2 e)]
       [{__ {'tody/name n}}
        (unify/trunk/semantic t1 (trunk->trunk* t2 e) e)]
       ;; about var
       [{{'tody/var v} __}
        (match (compute/var v e)
          [{'fail il} {'fail il}]
          [{'success {(d . __) __ __}}
           (match d
             [{'arrow a}
              (unify/trunk/semantic
               {a1 {'tody/arrow-list {a}} dl1 i1} t2 e)]
             [{'lambda {a al}}
              (unify/trunk/semantic
               {a1 {'tody/arrow-list al} dl1 i1} t2 e)]
             [__
              {'fail {`(unify/trunk/semantic
                        a var computes to neither arrow nor lambda
                        (var: ,v))}}])])]
       [{__ {'tody/var v}}
        (match (compute/var v e)
          [{'fail il} {'fail il}]
          [{'success {(d . __) __ __}}
           (match d
             [{'arrow a}
              (unify/trunk/semantic
               t1 {a2 {'tody/arrow-list {a}} dl2 i2} e)]
             [{'lambda {a al}}
              (unify/trunk/semantic
               t1 {a2 {'tody/arrow-list al} dl2 i2} e)]
             [__
              {'fail {`(unify/trunk/semantic
                        a var computes to neither arrow nor lambda
                        (var: ,v))}}])])]
       ;; about arrow-list
       [{{'tody/arrow-list al1} {'tody/arrow-list al2}}
        ;; recur to unify/data
        ;; only when at least one of the trunk is reducible
        ;; and if the arguments of this recur are both trunk
        ;; one of them may still be reducible
        ;; thus will get in to this branch again
        (match {(filter-arrow-list al1 dl1 e)
                (filter-arrow-list al2 dl2 e)}
          [{l1 l2}
           (if (not (or (eq? 1 (length l1)) (eq? 1 (length l2))))
             {'fail {`(unify/trunk/semantic
                       fail both trunks are non-reducible
                       (trunk1: ,t1)
                       (trunk2: ,t2))}}
             (match {(compute/trunk t1 e)
                     (compute/trunk t2 e)}
               [{{'success {(d1 . __) __ __}}
                 {'success {(d2 . __) __ __}}}
                (unify/data d1 d2 e)]
               [{__ __}
                {'fail {`(unify/trunk/semantic
                          fail to compute/trunk one of the trunks
                          (trunk1: ,t1)
                          (trunk2: ,t2))}}]))])])]))

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

(define init-env
  '(()
    ()
    ((type . (cons/type ((()
                          (cons (type ())))
                         type
                         type))))))

(define-syntax eva
  (syntax-rules ()
    [(eva e ...)
     (eva/top-list (map parse/top (quote (e ...))) init-env)]))

(define (eva/top-list tl e)
  (: (top ...) env -> env)
  (match tl
    [{} e]
    [(t . r) (eva/top-list r (eva/top t e))]))

(define (parse/top s)
  (: sexp-top -> top)
  (match s
    [('deftype n a . body)
     {'deftype {{n a} (parse/top/deftype-body body)}}]
    [('defn n a . al)
     {'defn {{n a} al}}]
    [{'app a}
     {'app a}]))

(define (parse/top/deftype-body body)
  (: deftype-body -> ((form1/name form1/arrow) ...))
  (cond [(eq? '() body) '()]
        [(eq? '() (cdr body))
         (orz 'parse/top/deftype-body ("wrong body : ~a~%" body))]
        [else
         (cons (list (car body) (cadr body))
               (parse/top/deftype-body (cddr body)))]))

(define (eva/top t e)
  (: top env -> env)
  (match t
    [{'deftype deftype} (eva/deftype deftype e)]
    [{'defn defn} (eva/defn defn e)]
    [{'app a} (eva/app a e)]))

(define (form1/arrow->arrow a e)
  (: form1/arrow env -> arrow)
  (match (pass2/arrow (pass1/arrow 0 a) {})
    [{a1 s} (pass3/get-arrow a1 e)]))

(define (form1/arrow->arrow-check ta a e)
  (: form1/arrow env -> arrow)
  (match (pass2/arrow (pass1/arrow 0 a) {})
    [{a1 s} (pass3/get-arrow-check ta a1 e)]))

(define (eva/app a e)
  (: form1/arrow env -> env)
  (let ([a0 (form1/arrow->arrow a e)])
    (match (compute/arrow a0 e)
      [{'success e1} e1]
      [{'fail il}
       (cat ("eva/app fail~%"))
       (pretty-print il)
       (cat ("~%"))
       (orz 'eva/ap ("end of report~%"))])))

(define (eva/deftype deftype e)
  (: ((form1/name form1/arrow) ((form1/name form1/arrow) ...)) env -> env)
  (match e
    [{ds bs ns}
     (match deftype
       [{{n a} nal}
        (let* ([nl (map car nal)]
               [a0 (form1/arrow->arrow a e)]
               [ns1 (cons (cons n
                                {'cons/type {a0 n nl}})
                          ns)])
          (eva/deftype/data-constructor-list n nal {ds bs ns1}))])]))

(define (eva/deftype/data-constructor type-name na e)
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

(define (eva/deftype/data-constructor-list type-name nal e)
  (: name ((form1/name form1/arrow) ...) env -> env)
  (match nal
    [{} e]
    [(na . r)
     (eva/deftype/data-constructor-list
      type-name r
      (eva/deftype/data-constructor type-name na e))]))

(define cover-check? #t)
(define (cover-check-) (set! cover-check? #f) #f)
(define (cover-check+) (set! cover-check? #t) #t)

(define recur-check? #t)
(define (recur-check-) (set! recur-check? #f) #f)
(define (recur-check+) (set! recur-check? #t) #t)

(define (eva/defn defn e)
  (: ((form1/name form1/arrow) (form1/arrow ...)) env -> env)
  (match e
    [{ds bs ns}
     (match defn
       [{{n a} al}
        (let* ([a0 (form1/arrow->arrow a e)]
               ;; need to put the type into ns first
               ;; for recursive call in arrow-list
               ;; that is
               ;; in ns
               ;; type global-bindings and arrow-list global-bindings
               ;; must be separately interfaced
               [ns0 (cons (cons n {'lambda {a0 'placeholder}}) ns)]
               [l1 {a0 (map (lambda (x)
                              (form1/arrow->arrow-check
                               a0 x {ds bs ns0}))
                         al)}]
               [ns1 (cons (cons n {'lambda l1}) ns)]
               [e1 {ds bs ns1}])
          (if cover-check?
            (match (cover-check l1 e1)
              [{'fail il} (orz 'eva/defn
                            ("info-list :~%~a~%" il))]
              [{'success __} 'ok]))
          (if recur-check?
            (match (recur-check n l1 e1)
              [{'fail il} (orz 'eva/defn
                            ("info-list :~%~a~%" il))]
              [{'success __} 'ok]))
          e1)])]))

(define (sequent)
  (: -> [loop])
  (cat ("welcome to sequent ^-^/~%"))
  (sequent/repl init-env))

(define (sequent/repl e)
  (: env -> [loop])
  (let* ([top (read)]
         [e1 (eva/top (parse/top top) e)])
    (match e1
      [{ds1 bs1 ns1}
       (print/data-list ds1 e1)
       (newline)
       (sequent/repl e1)])))

(define (type-compute/cedent c e)
  (: cedent env -> report)
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
  (match e
    [{ds bs ns}
     (match (copy-arrow a)
       ;; need to copy the arrow first
       ;; because the return arrow might be applied somewhere else
       [{ac sc}
        (match (type-compute/cedent ac {{} (cons '(commit-point) bs) ns})
          [{'fail il} {'fail il}]
          [{'success {ds1 bs1 ns1}}
           (match (type-compute/cedent sc {{} bs1 ns1})
             [{'fail il} {'fail il}]
             [{'success {ds2 bs2 ns2}}
              {'success {(cons {'arrow {(reverse ds1) (reverse ds2)}}
                               ds)
                         (bs/commit! bs2)
                         ns2}}])])])]))

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

(define (infer/arrow a e)
  (: arrow env -> arrow)
  (match (type-compute/arrow a e)
    [{'fail il}
     (orz 'infer/arrow
       ("fail to type-compute/arrow : ~a~%" a)
       ("reported info-list : ~a~%" il))]
    [{'success {(a1 . __) __ __}}
     a1]))

(define (infer/arrow-list al e)
  (: (arrow ...) env -> arrow)
  (unite/arrow-list
   (map (lambda (x) (infer/arrow x e)) al)
   e))

(define (unite/arrow-list al e)
  (: (arrow ...) env -> arrow)
  (letrec ([recur
            (lambda (a l)
              (: arrow (arrow ...) -> arrow)
              (match l
                [{} a]
                [(h . r)
                 (recur (unite/two a h e) r)]))])
    (recur (car al) (cdr al))))

(define (unite/two a1 a2 e)
  (: arrow arrow env -> arrow)
  (match e
    [{ds bs ns}
     (match {a1 a2}
       [{{ac1 sc1} {ac2 sc2}}
        (let ([ac1 (copy-arrow ac1)]
              [sc1 (copy-arrow sc1)]
              [ac2 (copy-arrow ac2)]
              [sc2 (copy-arrow sc2)])
          (match (unify/data-list
                  ac1 ac2
                  {'success {{} (cons '(commit-point) bs) ns}})
            [{'fail il} (orz 'unite/two
                          ("fail to unify antecedent~%")
                          ("ac1 : ~a~%" ac1)
                          ("ac2 : ~a~%" ac2))]
            [{'success {__ bs1 ns1}}
             (match (unify/data-list
                     sc1 sc2
                     {'success {{} bs1 ns1}})
               [{'fail il} (orz 'unite/two
                             ("fail to unify succedent~%")
                             ("sc1 : ~a~%" sc1)
                             ("sc2 : ~a~%" sc2))]
               [{'success {ds2 bs2 ns2}}
                (bs/commit! bs2)
                {ac1 sc1}])]))])]))

(define (cover-check l e)
  (: lambda env -> report)
  (match e
    [{ds bs ns}
     (match l
       [{{ac __} al}
        (match (data-gen ac (map car al) e)
          [{c bsl}
           (let ([report-list
                  (filter
                   (lambda (r)
                     (match r
                       [{'fail __} #t]
                       [{'success __} #f]))
                   (map (lambda (bs0)
                          (cover-check/cedent
                           c al
                           {ds (append bs0 bs) ns}))
                     bsl))])
             (if (null? report-list)
               {'success e}
               {'fail
                {`(cover-check
                   fail
                   (report-list: ,report-list:))}}))])])]))

(define (cover-check/cedent c al e)
  (: cedent (arrow ...) env -> report)
  )



(define (data-gen ac acl e)
  (: antecedent (antecedent ...) env -> (cedent (bs ...)))
  (alter-expand (data-gen/cedent ac acl e)))

(define (alter-expand alterdata-list)
  (: (alterdata ...) -> ((data ...) (bs ...)))
  )

(define (data-gen/cedent c cl e)
  (: cedent (cedent ...) env -> (alterdata ...))
  (match c
    [{} {}]
    [(h . r)
     (cons (data-gen/data h (map car cl) e)
           (data-gen/cedent r (map cdr cl) e))]))

(define (data-gen/data d dl e)
  (: data (data ...) env -> alterdata)
  (match d
    [{'var x} (data-gen/var x dl e)]
    [{'cons x} (data-gen/cons x dl e)]
    [{'arrow x} (data-gen/arrow x dl e)]
    [{'lambda x} (data-gen/lambda x dl e)]
    [{'trunk x} (data-gen/trunk x dl e)]))

(define (data-gen/cons c dl e)
  (: cons (data ...) env -> alterdata)
  (match c
    ;; c must be a type constructor
    ;; c must not be a data constructor
    [{n dl0}
     (if (list-any? (lambda (x)
                      (match x
                        [{'var v} (var/fresh? v e)]
                        [__ #f]))
                    dl)
       {'altervar (cons (id/new (symbol-append n ':gen)
                                {(cons 1 {'cons c})})
                        '())}
       '(>< return an altervar

           whoes type is bind to the cons as type
           whoes alterdata-list is not empty

           the alterdata-list is generate by recursive call to data-gen/data

           from the type constructor c
           we get a list of data constructor

           each must be covered by the dl

           for each that is covered by the dl
           the antecedent of the type arrow of the data constructor
           is used to do a recursive call to data-gen/data))]))

(define (data-gen/var v dl e)
  (: var (data ...) env -> alterdata)
  (match e
    [{ds bs ns}
     (if (not (var/fresh? v e))
       (data-gen/data (bs/deep bs {'var v}) dl e)
       '(><><><))]))

(define (data-gen/arrow a dl e)
  (: arrow (data ...) env -> alterdata)
  )

(define (data-gen/trunk l dl e)
  (: trunk (data ...) env -> alterdata)
  )

(define (data-gen/lambda l dl e)
  (: lambda (data ...) env -> alterdata)
  (orz 'data-gen/lambda
    ("can not generate data from lambda~%")
    ("l : ~a~%" l)
    ("dl : ~a~%" dl)))

(define (recur-check n l e)
  (: name lambda env -> report)
  (match e
    [{ds bs ns}
     {'success e}]))
