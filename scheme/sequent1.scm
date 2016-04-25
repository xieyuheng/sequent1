;; module system of guile
;; using http://synthcode.com/scheme/match.scm
(use-modules (ice-9 match))

;; guile
(define-syntax cat
  (syntax-rules ()
    [(cat (str . args))
     (format #t str . args)]
    [(cat (str . args) (str2 . args2) ...)
     (string-append
      (cat (str . args))
      (cat (str2 . args2) ...))]))

;; (define-syntax cat
;;   (syntax-rules ()
;;     [(cat (str . args))
;;      (format str . args)]
;;     [(cat (str . args) (str2 . args2) ...)
;;      (string-append
;;       (cat (str . args))
;;       (cat (str2 . args2) ...))]))

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

(define (pass1/arrow default-level s)
  ;; default-level, form1/arrow -> form2/arrow
  (list (pass1/cedent default-level (left-of '-> s))
        (pass1/cedent default-level (right-of '-> s))))

(define (pass1/cedent default-level s)
  ;; default-level, (form1 ...) -> (form2 ...)
  (match s
    ['() '()]
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
    ['() (list '() s)]
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
