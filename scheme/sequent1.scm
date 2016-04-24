;; module system of guile
;; using http://synthcode.com/scheme/match.scm
(use-modules (ice-9 match))

(define-syntax cat
  (syntax-rules ()
    [(cat (str . args))
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

(define (parse/arrow s)
  ;; sexp-arrow -> formal-arrow
  (list (parse/cedent 0 (left-of '-> s))
        (parse/cedent 0 (right-of '-> s))))

(define (parse/cedent default-level s)
  ;; default-level, sexp-cedent -> formal-cedent
  (match s
    ['() '()]
    [(h . r) (cons (parse/dispatch default-level h)
                   (parse/cedent default-level r))]))

(define (parse/dispatch default-level v)
  ;; default-level, sexp-form -> formal-form
  (let ([var? (lambda (v)
                (and (symbol? v)
                     (equal? ":" (substring (symbol->string v) 0 1))))]
        [name? (lambda (v)
                 (and (symbol? v)
                      (not (eq? ":" (substring (symbol->string v) 0 1)))))]
        [arrow? (lambda (v) (and (list? v) (member '-> v)))]
        [im-bind? (lambda (v) (and (list? v) (member ': v)))]
        [ex-bind? (lambda (v) (and (list? v) (member '@ v)))])
    (cond [(var? v) (list 'v (parse/var default-level v))]
          [(name? v) (list 'n v)]
          [(arrow? v) (list 'a (parse/arrow v))]
          [(im-bind? v) (list 'b
                              (list (parse/cedent 1 (left-of ': v))
                                    (parse/cedent 0 (right-of ': v))
                                    #f))]
          [(ex-bind? v) (list 'b
                              (list (parse/cedent 1 (left-of '@ v))
                                    (parse/cedent 0 (right-of '@ v))
                                    #t))]
          [else (orz ("parse/dispatch can not handle sexp-form:~a" v))])))

(define (parse/var default-level v)
  ;; default-level, symbol -> formal-var
  (let* ([str (symbol->string v)]
         [cursor (find-char "^" str)])
    (if cursor
      (list (string->symbol (substring str 0 cursor))
            (string->number (substring str (+ 1 cursor))))
      (list v default-level))))

(test
 (list
  (parse/arrow '(natural natural -> natural))
  (parse/arrow '(natural natural -> (natural natural -> natural) natural))
  (parse/arrow '(:m zero -> :m))
  (parse/arrow '(:m :n succ -> :m :n recur succ))
  (parse/arrow '((:t : type) :t -> type))
  (parse/arrow '((:t @ type) :t -> type))
  (parse/arrow '((:t^2 : type) :t -> type))
  (parse/arrow '((:t1 :t2^2 :t3^0 : j k) :t -> type))
  (parse/arrow '((:t^2 @ type) :t -> type)))
 '((((n natural) (n natural)) ((n natural)))
   (((n natural) (n natural)) ((a (((n natural) (n natural)) ((n natural)))) (n natural)))
   (((v (:m 0)) (n zero)) ((v (:m 0))))
   (((v (:m 0)) (v (:n 0)) (n succ)) ((v (:m 0)) (v (:n 0)) (n recur) (n succ)))
   (((b (((v (:t 1))) ((n type)) #f)) (v (:t 0))) ((n type)))
   (((b (((v (:t 1))) ((n type)) #t)) (v (:t 0))) ((n type)))
   (((b (((v (:t 2))) ((n type)) #f)) (v (:t 0))) ((n type)))
   (((b (((v (:t1 1)) (v (:t2 2)) (v (:t3 0))) ((n j) (n k)) #f)) (v (:t 0))) ((n type)))
   (((b (((v (:t 2))) ((n type)) #t)) (v (:t 0))) ((n type)))))

(define (pass1/arrow f s)
  ;; formal-arrow, scope -> arrow
  (match f
    [(fac fsc)
     (match (pass1/cedent fac s)
       [(ac s0)
        (match (pass1/cedent fsc s0)
          [(sc s1)
           (list ac sc)])])]))

(define (pass1/cedent f s)
  ;; formal-cedent, scope -> (cedent scope)
  (match f
    ['() (list '() s)]
    [(h . r)
     (match (pass1/dispatch h s)
       [(v s0)
        (match (pass1/cedent r s0)
          [(c s1)
           (list (cons v c) s1)])])]))

(define (pass1/dispatch f s)
  ;; formal-form, scope -> (form scope)
  (match f
    [('v v) (pass1/var v s)]
    [('n n) (list (list 'name n) s)]
    [('a a) (list (list 'arrow (pass1/arrow a s)) s)]
    [('b b) (pass1/bind b s)]))

(define (pass1/var v s)
  ;; formal-var, scope -> (var scope)
  (match v
    [(symbol level)
     (let ([found (assq symbol s)])
       (if found
         (let ([old (cdr found)])
           (list (list 'var (list old level)) s))
         (let ([new (vector symbol '())])
           (list (list 'var (list new level))
                 (cons (cons symbol new) s)))))]))

(define (pass1/bind b s)
  ;; formal-bind, scope -> (bind scope)
  (match b
    [(fvs fc leave?)
     (match (pass1/cedent fvs s)
       [(vs s0)
        (match (pass1/cedent fc s0)
          ;; this means vars in fvs can occur in fc
          [(c s1)
           (list (list 'bind (list vs c leave?)) s1)])])]))

(test
 (map (lambda (x) (pass1/arrow x '()))
   (list
    (parse/arrow '(natural natural -> natural))
    (parse/arrow '(natural natural -> (natural natural -> natural) natural))
    (parse/arrow '(:m zero -> :m))
    (parse/arrow '(:m :n succ -> :m :n recur succ))
    (parse/arrow '((:t : type) :t -> type))
    (parse/arrow '((:t @ type) :t -> type))
    (parse/arrow '((:t^2 : type) :t -> type))
    (parse/arrow '((:t1 :t2^2 :t3^0 : j k) :t -> type))
    (parse/arrow '((:t^2 @ type) :t -> type))))
 '((((name natural) (name natural))
    ((name natural)))
   (((name natural) (name natural))
    ((arrow (((name natural) (name natural))
             ((name natural))))
     (name natural)))
   (((var (#(:m ()) 0)) (name zero))
    ((var (#(:m ()) 0))))
   (((var (#(:m ()) 0)) (var (#(:n ()) 0)) (name succ))
    ((var (#(:m ()) 0)) (var (#(:n ()) 0)) (name recur) (name succ)))
   (((bind (((var (#(:t ()) 1))) ((name type)) #f)) (var (#(:t ()) 0)))
    ((name type)))
   (((bind (((var (#(:t ()) 1))) ((name type)) #t)) (var (#(:t ()) 0)))
    ((name type)))
   (((bind (((var (#(:t ()) 2))) ((name type)) #f)) (var (#(:t ()) 0)))
    ((name type)))
   (((bind (((var (#(:t1 ()) 1)) (var (#(:t2 ()) 2)) (var (#(:t3 ()) 0))) ((name j) (name k)) #f)) (var (#(:t ()) 0)))
    ((name type)))
   (((bind (((var (#(:t ()) 2))) ((name type)) #t)) (var (#(:t ()) 0)))
    ((name type)))))

(define (apply/arrow a e)
  ;; arrow, env -> report
  (match e
    [(ds bs ns)
     (match a
       [(ac sc)
        (match (unify (lambda (e) (apply/cedent ac e))
                      (list ds
                            (cons '(commit-point) bs)
                            ns))
          [('fail info-list) ('fail info-list)]
          [('success info-list e1)
           (match (apply/cedent sc e1)
             [(ds2 bs2 ns2)
              (list 'success info-list
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

(define (id/commit! id ls)
  ;; id, ls -> id
  ;; effect on id
  (let ()
    (vector-set! id (append ls (vector-ref id 1)))
    id))

(define (apply/cedent c e)
  ;; cedent, env -> env
  (match c
    ['() e]
    [(h . r) (apply/cedent r (apply/dispatch h e))]))

(define (apply/dispatch f e)
  ;; form, env -> env
  (match f
    [('var v) (apply/var v e)]
    [('name n) (apply/name n e)]
    [('arrow a) (apply/literal-arrow a e)]
    [('bind b) (apply/bind b e)]))

(define (id->ls id)
  (vector-ref id 1))

(define (unify e)
  ;; (env -> env), env -> unify-report
  )
