;; module system of guile
;; using http://synthcode.com/scheme/match.scm
(use-modules (ice-9 match))

(define-syntax cat
  (syntax-rules ()
    [(cat (str . args))
     ;; (format str . args)
     ;; guile
     (format #t str . args)]
    [(cat (str . args) (str2 . args2) ...)
     (string-append
      (cat (str . args))
      (cat (str2 . args2) ...))]))

(define-syntax orz
  (syntax-rules ()
    [(orz . body)
     (error (cat . body))]))

(define-syntax note
  (syntax-rules ()
    [(note . body)
     '()]))

(use-modules (ice-9 pretty-print))

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
         (orz ("<test-fail-report-end>\n"))))]))

(define (left-of s l)
  ;; sexp, list -> list
  (cond [(equal? s (car l)) '()]
        [else (cons (car l) (left-of s (cdr l)))]))

(define (right-of s l)
  ;; sexp, list -> list
  (cond [(equal? s (car l)) (cdr l)]
        [else (right-of s (cdr l))]))

(define (sublist l start end)
  ;; list, index, index -> list
  (cond [(and (eq? 0 start) (<= end 0)) '()]
        [(and (not (eq? 0 start)))
         (sublist (cdr l) (- start 1) (- end 1))]
        [(and (eq? 0 start) (not (eq? 0 end)))
         (cons (car l) (sublist (cdr l) 0 (- end 1)))]))

(define (genlist len)
  ;; length -> list
  (define (recur len counter)
    (cond [(eq? len counter) '()]
          [else (cons counter
                      (recur len (+ 1 counter)))]))
  (recur len 0))

(define (substitute e p? l)
  ;; element, (element -> bool), (element ...) -> (element ...)
  (cond [(eq? '() l) '()]
        [(p? (car l)) (cons e (cdr l))]
        [else (cons (car l) (substitute e p? (cdr l)))]))

(define (find-char c s)
  ;; char, string -> curser or #f
  (find-char/curser c s 0))

(define (find-char/curser c s curser)
  ;; char, string, curser -> curser or #f
  (if (>= curser (string-length s))
    #f
    (let ([c0 (substring s curser (+ 1 curser))])
      (if (equal? c c0)
        curser
        (find-char/curser c s (+ 1 curser))))))

(define (pass1/arrow default-level s)
  ;; default-level, form1/arrow -> form2/arrow
  (list (pass1/cedent default-level (left-of '-> s))
        (pass1/cedent default-level (right-of '-> s))))

(define (pass1/cedent default-level s)
  ;; default-level, (form1 ...) -> (form2 ...)
  (match s
    [() '()]
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
  ;; default-level, form1 -> form2
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
                     'leave))]
        [(form1/ex-bind? v)
         (list 'form2/bind
               (list (pass1/cedent 1 (left-of '@ v))
                     (pass1/cedent 0 (right-of '@ v))
                     'not-leave))]
        [else
         (orz ("pass1 can not handle sexp-form:~a" v))]))

(define (pass1/var default-level v)
  ;; default-level, symbol -> form2/var
  (let* ([str (symbol->string v)]
         [cursor (find-char "^" str)])
    (if cursor
      (list (string->symbol (substring str 0 cursor))
            (string->number (substring str (+ 1 cursor))))
      (list v default-level))))

(define (pass2/arrow a s)
  ;; form2/arrow, scope -> (form3/arrow scope)
  (match a
    [(ac sc)
     (match (pass2/cedent ac s)
       [(3ac s1)
        (match (pass2/cedent sc s1)
          [(3sc s2)
           (list (list 3ac 3sc) s2)])])]))

(define (pass2/cedent c s)
  ;; (form2 ...), scope -> ((form3 ...) scope)
  (match c
    [() (list '() s)]
    [(h . r)
     (match (pass2 h s)
       [(3f s1)
        (match (pass2/cedent r s1)
          [(3c s2)
           (list (cons 3f 3c) s2)])])]))

(define (pass2/lambda l s)
  ;; form2/lambda, scope -> (form3/lambda scope)
  (match l
    [(a al)
     (list (list (pass2/arrow a s)
                 (map (lambda (x) (pass2/arrow x s))
                   al))
           s)]))

(define (pass2 f s)
  ;; form2, scope -> (form2 scope)
  (match f
    [('form2/var v)
     (match (pass2/var v s)
       [(v1 s1)
        (list (list 'form3/var v1) s1)])]
    [('form2/name n)
     (list (list 'form3/name n) s)]
    [('form2/arrow a)
     (match (pass2/arrow a s)
       [(a1 s1)
        (list (list 'form3/arrow a1) s1)])]
    [('form2/lambda l)
     (match (pass2/lambda l s)
       [(l1 s1)
        (list (list 'form3/lambda l1) s1)])]
    [('form2/bind b)
     (match (pass2/bind b s)
       [(b1 s1)
        (list (list 'form3/bind b1) s1)])]))

(define (pass2/var v s)
  ;; form2/var, scope -> (form3/var scope)
  (match v
    [(symbol level)
     (let ([found (assq symbol s)])
       (if found
         (let ([old (cdr found)])
           (list (list old level)
                 s))
         (let ([new (vector symbol '())])
           (list (list new level)
                 (cons (cons symbol new) s)))))]))

(define (pass2/bind b s)
  ;; form2/bind, scope -> (form3/bind scope)
  (match b
    [(vs c leave?)
     (match (pass2/cedent vs s)
       [(3vs s1)
        (match (pass2/cedent c s1)
          ;; this means vars in vs can occur in c
          [(3c s2)
           (list (list 3vs 3c leave?) s2)])])]))

(define (pass3/get-arrow a e)
  ;; form3/arrow, env -> arrow
  (match (pass3/arrow a e)
    [((('arrow arrow) . _) _ _)
     arrow]))

(define (pass3/arrow a e)
  ;; form3/arrow, env -> env
  (match e
    [(ds bs ns)
     (match a
       [(ac sc)
        (match (pass3/cedent ac e)
          [((d1 . _) _ _)
           (match (pass3/cedent sc e)
             [((d2 . _) _ _)
              (list (cons (list 'arrow (list d1 d2))
                          ds)
                    bs
                    ns)])])])]))

(define (pass3/cedent c e)
  ;; (form3 ...), env -> env
  (match e
    [(ds bs ns)
     (match c
       [() e]
       [(h . r) (pass3/cedent r (pass3 h e))])]))

(define (pass3/lambda l e)
  ;; form3/lambda, env -> env
  (match e
    [(ds bs ns)
     (match l
       [(a al)
        (list (cons (list 'lambda
                          (pass3/get-arrow a e)
                          (map (lambda (x)
                                 (pass3/get-arrow x e))
                            al))
                    ds)
              bs
              ns)])]))

(define (pass3 f e)
  ;; form3, env -> env
  (match f
    [('form3/var x) (pass3/var x e)]
    [('form3/name x) (pass3/name x e)]
    [('form3/arrow x) (pass3/arrow x e)]
    [('form3/lambda x) (pass3/lambda x e)]
    [('form3/bind x) (pass3/bind x e)]))

(define (pass3/var v e)
  ;; form3/var, env -> env
  (match e
    [(ds bs ns)
     ;; actually there is no need to search bs
     ;; but anyway
     (list (cons (bs/deep bs (list 'var v)) ds)
           bs
           ns)]))

(define (id->symbol id)
  (vector-ref id 0))

(define (id->ls id)
  (vector-ref id 1))

(define (bs/find bs v)
  ;; bs, var -> data or #f
  (match v
    [(id level)
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
  ;; bs, data -> data
  (match d
    [('var v)
     (let ([found (bs/find bs v)])
       (if found
         (bs/walk bs found)
         d))]
    [(_ e) d]))

(define (bs/deep bs d)
  ;; bs, data -> data
  (define (bs/deep-list bs dl)
    (map (lambda (x) (bs/deep bs x)) dl))
  (define (bs/deep-arrow bs a)
    (match a
      [(dl1 dl2)
       (list (bs/deep-list bs dl1)
             (bs/deep-list bs dl2))]))
  (define (bs/deep-arrow-list bs al)
    (map (lambda (a) (bs/deep-arrow bs a)) al))
  (match (bs/walk bs d)
    [('var v) ('var v)]
    [('cons (name dl))
     (list 'cons
           (list name (bs/deep-list bs dl)))]
    [('arrow a) (list 'arrow (bs/deep-arrow bs a))]
    [('lambda (a al))
     (list 'lambda
           (list (bs/deep-arrow bs a)
                 (bs/deep-arrow-list bs al)))]
    [('trunk (a al dl i))
     (list 'trunk
           (list (bs/deep-arrow bs a)
                 (bs/deep-arrow-list bs al)
                 (bs/deep-list bs dl)
                 i))]))

(define (pass3/name n e)
  ;; form3/name, env -> env
  (match e
    [(ds bs ns)
     (let ([found (assq n ns)])
       (if (not found)
         (orz ("pass3/name unknow name : ~a~%" n))
         (let ([meaning (cdr found)])
           (match meaning
             [('cons/type ((ac sc) name _))
              (pass3/name/cons (length ac) name e)]
             [('cons/data ((ac sc) name _))
              (pass3/name/cons (length ac) name e)]
             [('lambda ((ac sc) al))
              (pass3/name/trunk (length ac) (length sc) (cadr meaning) e)]))))]))

(define (pass3/name/cons len name e)
  ;; length, name, env -> env
  (match e
    [(ds bs ns)
     (list (cons (list 'cons
                       (list name (sublist ds 0 len)))
                 (sublist ds len -1))
           bs
           ns)]))

(define (pass3/name/trunk len slen l e)
  ;; length, length, lambda, env -> env
  (match e
    [(ds bs ns)
     (match l
       [(a al)
        (let* ([a (copy-arrow a)]
               [al (map copy-arrow al)]
               [dl (sublist ds 0 len)]
               [make-trunk (lambda (i) (list 'trunk (list a al dl i)))])
          (list (append (map make-trunk (genlist slen))
                        (sublist ds len -1))
                bs
                ns))])]))

(define (pass3/bind b e)
  ;; form3/bind, env -> env
  (match b
    [(vl c leave?)
     (match (pass3/cedent c e)
       [((d1 . _) _ _) ;; here I assume the c of bind is simple
        (letrec ([recur
                  (lambda (vl e)
                    (match (list vl e)
                      [(() _) e]
                      [(((id level) . r) (ds bs ns))
                       ;; ><><><
                       ;; need to check if the bind already exist
                       ;; and to check type
                       (id/commit! id (list (cons level d1)))
                       (recur r (list (if leave?
                                        (cons d1 ds)
                                        ds)
                                      bs
                                      ns))]))])
          (recur vl e))])]))

(define (id/commit! id ls)
  ;; id, ls -> id
  ;; effect on id
  (let ()
    (vector-set! id 1 (append ls (vector-ref id 1)))
    id))

(define (copy-arrow a)
  ;; arrow -> arrow
  (match (copy/arrow a '())
    [(a s) a]))

(define (copy/arrow a s)
  ;; arrow, scope -> (arrow scope)
  (match a
    [(ac sc)
     (match (copy/cedent ac s)
       [(ac1 s1)
        (match (copy/cedent sc s1)
          [(sc1 s2)
           (list ac1 sc1 s2)])])]))

(define (copy/cedent c s)
  ;; (data ...), scope -> ((data ...) scope)
  (match c
    [() (list '() s)]
    [(h . r)
     (match (copy h s)
       [(h1 s1)
        (match (copy/cedent r s1)
          [(r1 s2)
           (list (cons h1 r1) s2)])])]))

(define (copy/lambda l s)
  ;; lambda, scope -> (lambda scope)
  (match l
    [(a al)
     (match (copy/arrow a s)
       [(a1 s1)
        (match (copy/arrow-list al s1)
          [(al1 s2)
           (list (list a1 al1) s2)])])]))

(define (copy/arrow-list al s)
  ;; (arrow ...), scope -> ((arrow ...) scope)
  (match al
    [() (list '() s)]
    [(h . r)
     (match (copy/arrow h s)
       [(h1 s1)
        (match (copy/arrow-list r s1)
          [(r1 s2)
           (list (cons h1 r1) s2)])])]))

(define (copy d s)
  ;; data, scope -> (data scope)
  (match d
    [('var x)
     (match (copy/var x s)
       [(x1 s1)
        (list (list 'var x1) s1)])]
    [('cons x)
     (match (copy/cons x s)
       [(x1 s1)
        (list (list 'cons x1) s1)])]
    [('arrow x)
     (match (copy/arrow x s)
       [(x1 s1)
        (list (list 'arrow x1) s1)])]
    [('lambda x)
     (match (copy/lambda x s)
       [(x1 s1)
        (list (list 'lambda x1) s1)])]
    [('trunk x)
     (match (copy/trunk x s)
       [(x1 s1)
        (list (list 'trunk x1) s1)])]))

(define (copy/var v s)
  ;; var, scope -> (var scope)
  (match v
    [(id level)
     (let ([found (assq id s)])
       (if found
         (list (list (cdr found) level) s)
         (let* ([ls (id->ls id)]
                [id1 (vector (id->symbol id) '())]
                [s1 (cons (cons id id1) s)])
           (match (copy/ls ls s1)
             [(ls1 s2)
              (id/commit! id1 ls1)
              (list (list id1 level) s2)]))))]))

(define (copy/ls ls s)
  ;; ls, scope -> (ls scope)
  (match ls
    [() (list '() s)]
    [((level . data) . r)
     (match (copy data s)
       [(data1 s1)
        (match (copy/ls r s1)
          [(r1 s2)
           (list (cons (cons level data1)
                       r1)
                 s2)])])]))

(define (copy/cons c s)
  ;; cons, scope -> (cons scope)
  (match c
    [(n dl)
     (match (copy/cedent dl s)
       [(dl1 s1)
        (list (list n dl1) s1)])]))

(define (copy/trunk p s)
  ;; trunk, scope -> (trunk scope)
  (match p
    [(a al dl i)
     (match (copy/arrow a s)
       [(a1 s1)
        (match (copy/arrow-list al s1)
          [(al1 s2)
           (match (copy/cedent dl s2)
             [(dl1 s3)
              (list (list a1 al1 dl1 i) s3)])])])]))

(define (compute-arrow a e)
  ;; arrow, env -> report
  (match e
    [(ds bs ns)
     (match a
       [(ac sc)
        (match (unify (lambda (e) (compute/cedent ac e))
                      (list ds
                            (cons '(commit-point) bs)
                            ns))
          [('fail info-list)
           (list 'fail
                 (cons `(compute-arrow fail (arrow ,a)) info-list))]
          [('success e1)
           (match (compute/cedent sc e1)
             [(ds2 bs2 ns2)
              (list 'success
                    (list ds2 (bs/commit! bs2) ns2))])])])]))

(define (bs/commit! bs)
  ;; bs -> bs
  ;; effect on part of bs
  (cond [(equal? '(commit-point) (car bs))
         (cdr bs)]
        [else
         (let* ([pair (car bs)]
                [id (car pair)]
                [ls (cdr pair)])
           (id/commit! id ls)
           (bs/commit! (cdr bs)))]))

(define (compute/cedent c e)
  ;; cedent, env -> env
  (match c
    [() e]
    [(h . r) (compute/cedent r (compute h e))]))

(define (compute f e)
  ;; data, env -> env
  (match e
    [(ds bs ns)
     (list (cons f ds) bs ns)]))

(define (unify f e)
  ;; (env -> env), env -> report
  (match e
    [(ds bs ns)
     (match (f (list (cons 'unify-point ds) bs ns))
       [(ds1 bs1 ns1)
        (let* ([pl (left-of 'unify-point ds1)]
               [tmp (right-of 'unify-point ds1)]
               [len (length pl)]
               [dl (sublist tmp 0 len)]
               [ds2 (sublist tmp len -1)])
          (unify/data-list pl dl
                      (list 'success (list ds2 bs ns))))])]))

(define (unify/data-list pl dl r)
  ;; (pattern ...), (data ...), report -> report
  (match r
    [('fail info-list) ('fail info-list)]
    [('success e)
     (if (eq? pl '())
       r
       (unify/data-list
        (cdr pl) (cdr dl)
        (unify/data (car pl) (car dl) e)))]))

(define (var/eq? v1 v2)
  (match (list v1 v2)
    [((id1 level1) (id2 level2))
     (and (eq? id1 id2)
          (eq? level1 level2))]))

(define (unify/data p d e)
  ;; pattern, data, env -> report
  (match e
    [(ds bs ns)
     ;; var -walk-> fresh-var
     (let ([p (bs/walk bs p)]
           [d (bs/walk bs d)])
       (match (list p d)
         [(('var v1) ('var v2))
          (if (var/eq? v1 v2)
            (list 'success e)
            (list 'success
                  (list ds
                        (bs/extend bs v1 d)
                        ns)))]
         [(('var v) _) (unify/var/data v d e)]
         [(_ ('var v)) (unify/var/data v p e)]

         [(('trunk t1) ('trunk t2)) (unify/trunk t1 t2 e)]
         [(('trunk t) _) (unify/trunk/data t d e)]
         [(_ ('trunk t)) (unify/trunk/data t p e)]

         [(('cons c1) ('cons c2)) (unify/cons c1 c2 e)]
         [(('arrow a1) ('arrow a2)) (unify/arrow a1 a2 e)]
         [(('lambda l1) ('lambda l2)) (unify/lambda l1 l2 e)]
         [(_ _)
          (list 'fail
                (list '(unify/data
                        fail to unify
                        (pattern: ,p) (data: ,d))))]))]))

(define (bs/extend bs v d)
  ;; bs, var, data -> bs
  (match v
    [(id level)
     (let ([found/ls (assq id bs)])
       (if found/ls
         (substitute (cons id (cons (cons level d)
                                    (cdr found/ls)))
                     (lambda (pair) (eq? (car pair) id))
                     bs)
         (cons (cons id (list (cons level d)))
               bs)))]))

(define (unify/var/data v d e)
  ;; var, data, env -> report
  (match e
    [(ds bs ns)
     (list 'success
           (list ds (bs/extend bs v d) ns))]))

(define (unify/cons c1 c2 e)
  ;; cons, cons, env -> report
  (match (list c1 c2)
    [((n1 dl1) (n2 dl2))
     (if (eq? n1 n2)
       (unify/data-list dl1 dl2 (list 'success e))
       (list 'fail
             (list `(unify/cons
                     fail (cons1: ,c1) (cons: ,c2)))))]))

(define (unify/arrow a1 a2 e)
  ;; arrow, arrow, env -> report
  (match (list a1 a2)
    [((ac1 sc1) (ac2 sc2))
     (match (unify/data-list ac1 ac2 (list 'success e))
       [('success e1)
        (unify/data-list sc1 sc2 (list 'success e1))]
       [('fail info-list)
        (list 'fail
              (cons `(unify/arrow
                      fail  (arrow1: ,a1) (arrow2: ,a2))
                    info-list))])]))

(define (unify/lambda l1 l2 e)
  ;; lambda, lambda, env -> report
  (match (list l1 l2)
    [((a1 al1) (a2 al2))
     (unify/arrow-list al1 al2 (unify/arrow a1 a2 e))]))

(define (unify/arrow-list al1 al2 r)
  ;; (arrow ...), (arrow ...), report -> report
  (match r
    [('fail info-list) ('fail info-list)]
    [('success e)
     (if (eq? al1 '())
       r
       (unify/arrow-list
        (cdr al1) (cdr al2)
        (unify/arrow (car al1) (car al2) e)))]))

(define (unify/trunk t1 t2 e)
  ;; trunk, trunk, env -> report
  (match (list t1 t2)
    [((a1 al1 dl1) (a2 al2 dl2))
     (unify/data-list dl1 dl2 (unify/lambda (list a1 al1) (list a2 al2) e))]))

(define (unify/trunk/data t d e)
  ;; trunk, data, env -> report
  (match e
    [(ds bs ns)
     (match t
       [(a al dl i)
        (let* ([dl1 (map (lambda (x) (bs/deep bs x)) dl)]
               [al1 (filter-arrow-list al dl1 e)])
          (match al1
            [()
             (list 'fail
                   (list `(unify/trunk/data
                           fail arrow-list filter to
                           ()
                           (trunk: ,t)
                           (data: ,d))))]
            [(a1)
             (match (compute-arrow a1 (list dl1 bs ns))
               ;; after this compute-arrow
               ;; binds are commited
               ;; then the old env is used
               [('success e1) (unify/data (proj i e1) d e)]
               [('fail info) ('fail info)])]
            [(a1 a2 . _)
             (list 'fail
                   (list `(unify/trunk/data
                           fail arrow-list filter to
                           (arrow-list: ,al1)
                           (trunk: ,t)
                           (data: ,d))))]))])]))

(define (proj i e)
  ;; index, env -> data
  (match e
    [(ds bs ns)
     (list-ref ds (- (length ds) i))]))

(define (filter-arrow-list al dl e)
  ;; (arrow ...), (data ...), env -> (arrow ...)
  (if (eq? '() al)
    '()
    (match e
      [(ds bs ns)
        (match (car al)
          [(ac sc)
           (match (unify (lambda (x) (compute/cedent ac x))
                         (list (append dl ds)
                               bs
                               ns))
             [('fail _)
              (filter-arrow-list (cdr al) dl e)]
             [('success e1)
              (cons (car al)
                    (filter-arrow-list (cdr al) dl e))])])])))
