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

(test
 (map (lambda (x) (pass1/arrow 0 x))
   '((natural natural -> natural)
     (natural natural -> (natural natural -> natural) natural)
     (:m zero -> :m)
     (:m :n succ -> :m :n recur succ)
     (:m :n succ -> :m :n (lambda (natural natural -> natural)
                            (:m :n succ -> :m :n recur succ)
                            (:m :n succ -> :m :n recur succ)))
     ((:t : type) :t -> type)
     ((:t @ type) :t -> type)
     ((:t^2 : type) :t -> type)
     ((:t1 :t2^2 :t3^0 : j k) :t -> type)
     ((:t^2 @ type) :t -> type)))
 '((((form2/name natural) (form2/name natural))
    ((form2/name natural)))
   (((form2/name natural) (form2/name natural))
    ((form2/arrow (((form2/name natural) (form2/name natural)) ((form2/name natural)))) (form2/name natural)))
   (((form2/var (:m 0)) (form2/name zero))
    ((form2/var (:m 0))))
   (((form2/var (:m 0)) (form2/var (:n 0)) (form2/name succ))
    ((form2/var (:m 0)) (form2/var (:n 0)) (form2/name recur) (form2/name succ)))
   (((form2/var (:m 0)) (form2/var (:n 0)) (form2/name succ))
    ((form2/var (:m 0)) (form2/var (:n 0)) (form2/lambda ((((form2/name natural) (form2/name natural)) ((form2/name natural))) ((((form2/var (:m 0)) (form2/var (:n 0)) (form2/name succ)) ((form2/var (:m 0)) (form2/var (:n 0)) (form2/name recur) (form2/name succ))) (((form2/var (:m 0)) (form2/var (:n 0)) (form2/name succ)) ((form2/var (:m 0)) (form2/var (:n 0)) (form2/name recur) (form2/name succ))))))))
   (((form2/bind (((form2/var (:t 1))) ((form2/name type)) leave)) (form2/var (:t 0)))
    ((form2/name type)))
   (((form2/bind (((form2/var (:t 1))) ((form2/name type)) not-leave)) (form2/var (:t 0)))
    ((form2/name type)))
   (((form2/bind (((form2/var (:t 2))) ((form2/name type)) leave)) (form2/var (:t 0)))
    ((form2/name type)))
   (((form2/bind (((form2/var (:t1 1)) (form2/var (:t2 2)) (form2/var (:t3 0))) ((form2/name j) (form2/name k)) leave)) (form2/var (:t 0)))
    ((form2/name type)))
   (((form2/bind (((form2/var (:t 2))) ((form2/name type)) not-leave)) (form2/var (:t 0)))
    ((form2/name type)))))

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

(test
 (map (lambda (x) (pass2/arrow x '()))
   (map (lambda (x) (pass1/arrow 0 x))
     '((natural natural -> natural)
       (natural natural -> (natural natural -> natural) natural)
       (:m zero -> :m)
       (:m :n succ -> :m :n recur succ)
       (:m :n succ -> :m :n (lambda (natural natural -> natural)
                              (:m :n succ -> :m :n recur succ)
                              (:m :n succ -> :m :n recur succ)))
       ((:t : type) :t -> type)
       ((:t @ type) :t -> type)
       ((:t^2 : type) :t -> type)
       ((:t1 :t2^2 :t3^0 : j k) :t -> type)
       ((:t^2 @ type) :t -> type))))
 '(((((form3/name natural) (form3/name natural)) ((form3/name natural))) ())
   ((((form3/name natural) (form3/name natural)) ((form3/arrow (((form3/name natural) (form3/name natural)) ((form3/name natural)))) (form3/name natural))) ())
   ((((form3/var (#(:m ()) 0)) (form3/name zero)) ((form3/var (#(:m ()) 0)))) ((:m . #(:m ()))))
   ((((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name succ)) ((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name recur) (form3/name succ))) ((:n . #(:n ())) (:m . #(:m ()))))
   ((((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name succ)) ((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/lambda (((((form3/name natural) (form3/name natural)) ((form3/name natural))) ((:n . #(:n ())) (:m . #(:m ())))) (((((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name succ)) ((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name recur) (form3/name succ))) ((:n . #(:n ())) (:m . #(:m ())))) ((((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name succ)) ((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name recur) (form3/name succ))) ((:n . #(:n ())) (:m . #(:m ()))))))))) ((:n . #(:n ())) (:m . #(:m ()))))
   ((((form3/bind (((form3/var (#(:t ()) 1))) ((form3/name type)) leave)) (form3/var (#(:t ()) 0))) ((form3/name type))) ((:t . #(:t ()))))
   ((((form3/bind (((form3/var (#(:t ()) 1))) ((form3/name type)) not-leave)) (form3/var (#(:t ()) 0))) ((form3/name type))) ((:t . #(:t ()))))
   ((((form3/bind (((form3/var (#(:t ()) 2))) ((form3/name type)) leave)) (form3/var (#(:t ()) 0))) ((form3/name type))) ((:t . #(:t ()))))
   ((((form3/bind (((form3/var (#(:t1 ()) 1)) (form3/var (#(:t2 ()) 2)) (form3/var (#(:t3 ()) 0))) ((form3/name j) (form3/name k)) leave)) (form3/var (#(:t ()) 0))) ((form3/name type))) ((:t . #(:t ())) (:t3 . #(:t3 ())) (:t2 . #(:t2 ())) (:t1 . #(:t1 ()))))
   ((((form3/bind (((form3/var (#(:t ()) 2))) ((form3/name type)) not-leave)) (form3/var (#(:t ()) 0))) ((form3/name type))) ((:t . #(:t ()))))))

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
    [('trunk (a al dl))
     (list 'trunk
           (list (bs/deep-arrow bs a)
                 (bs/deep-arrow-list bs al)
                 (bs/deep-list bs dl)))]
    [('proj (a al dl i))
     (list 'proj
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
              (if (eq? 1 (length sc))
                (pass3/name/trunk (length ac) l e)
                (pass3/name/proj (length ac) (length sc) l e))]))))]))

(define (pass3/name/cons len name e)
  ;; length, name, env -> env
  (match e
    [(ds bs ns)
     (list (cons (list 'cons
                       (list name (sublist ds 0 len)))
                 (sublist ds len -1))
           bs
           ns)]))

(define (pass3/name/trunk len l e)
  ;; length, lambda, env -> env
  (match e
    [(ds bs ns)
     (match l
       [(a al)
        (let ([a (copy-arrow a)]
              [al (map copy-arrow al)]
              [dl (sublist ds 0 len)])
          (list (cons (list 'trunk (list a al dl))
                      (sublist ds len -1))
                bs
                ns))])]))

(define (pass3/name/proj len slen l e)
  ;; length, length, lambda, env -> env
  (match e
    [(ds bs ns)
     (match l
       [(a al)
        (let* ([a (copy-arrow a)]
               [al (map copy-arrow al)]
               [dl (sublist ds 0 len)]
               [make-proj (lambda (i) (list 'proj (list a al dl i)))])
          (list (append (map make-proj (genlist slen))
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
    (vector-set! id (append ls (vector-ref id 1)))
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
        (list (list 'trunk x1) s1)])]
    [('proj x)
     (match (copy/proj x s)
       [(x1 s1)
        (list (list 'proj x1) s1)])]))

(define (copy/var v s)
  ;; var, scope -> (var scope)
  (match v
    [(id level)
     (let ([found (assq id scope)])
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

(define (copy/trunk t s)
  ;; trunk, scope -> (trunk scope)
  (match t
    [(a al dl)
     (match (copy/arrow a s)
       [(a1 s1)
        (match (copy/arrow-list al s1)
          [(al1 s2)
           (match (copy/cedent dl s2)
             [(dl1 s3)
              (list (list a1 al1 dl1) s3)])])])]))

(define (copy/proj p s)
  ;; proj, scope -> (proj scope)
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

;; (define (compute f e)
;;   ;; data, env -> env
;;   (match e
;;     [(ds bs ns)
;;      (match f
;;        [('var x) (list (cons (bs/deep bs f) ds) bs ns)]
;;        [('cons x) (list (cons f ds) bs ns)]
;;        [('arrow x) (list (cons f ds) bs ns)]
;;        [('lambda x) (list (cons f ds) bs ns)]
;;        [('trunk x) (list (cons f ds) bs ns)]
;;        [('proj x) (list (cons f ds) bs ns)])]))

(define (unify e)
  ;; (env -> env), env -> unify-report
  )

(defun bs/extend (default-level bs v d)
  ;; bs var data -> bs
  (match v
    (id level) =>
    (let* ((level (if (eq nil level)
                      default-level
                      level))
           (found/ls (assoc id bs :test #'eq)))
      (if found/ls
          (substitute (cons id (cons (cons level d)
                                     (cdr found/ls)))
                      (lambda (pair) (eq (car pair) id))
                      bs)
          (cons (cons id (list (cons level d)))
                bs)))))
