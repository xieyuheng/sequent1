(define-syntax cat
  (syntax-rules ()
    [(cat (str . args))
     (format str . args)]
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
     (void)]))

(define (as env) (car env))
(define (bs env) (cadr env))
(define (ns env) (caddr env))

(define (env/as env as) (list as (bs env) (ns env)))
(define (env/bs env bs) (list (as env) bs (ns env)))
(define (env/ns env ns) (list (as env) (bs env) ns))

(define-syntax seq
  (syntax-rules ()
    [(_ c ...) (seq/f (quote (c ...)) '(() () ()))]))

(define (seq/f body env)
  ;; body env -> env
  (if (null? body)
    env
    (seq/f (cdr body)
           (eva (car body) env))))

(define (eva exp env)
  ;; exp env -> env
  (case (car exp)
    [(deftype) (eva/deftype (cdr exp) env)]
    [(def) (eva/def (cdr exp) env)]
    [(def/without-check) (eva/def/without-check (cdr exp) env)]
    [(run) (run (cdr exp) env)]))

(define (eva/deftype body env)
  ;; ns -> new-ns
  (define type-name (car body))
  (define type-sequent (cadr body))
  (define data-list (cddr body))
  (define (recur1 l)
    (cond [(null? l) '()]
          [else (cons (car l) (recur1 (cddr l)))]))
  (define data-name-list (recur1 data-list))
  (define (recur2 l)
    (cond [(null? l) '()]
          [else (cons (list (car l) 'data-constructor
                            (list (cadr l))
                            type-name)
                      (recur2 (cddr l)))]))
  (env/ns env
          (append
           (recur2 data-list)
           (cons (list type-name 'type-constructor
                       (list type-sequent)
                       data-name-list)
                 (ns env)))))

(define (eva/def body env)
  ;; ns -> new-ns
  (define type-sequent (list (cadr body)))
  (define data-sequent (cddr body))
  (define new-env
    (env/ns env
            (cons (list (car body) 'function
                        type-sequent
                        data-sequent)
                  (ns env))))
  (define (recur1 l)
    (if (null? l)
      new-env
      (if (check (car type-sequent)
                 (car l)
                 new-env)
        (recur1 (cdr l))
        #f)))
  (recur1 data-sequent))

(define (eva/def/without-check body env)
  ;; ns -> new-ns
  (define type-sequent (list (cadr body)))
  (define data-sequent (cddr body))
  (define new-env
    (env/ns env
            (cons (list (car body) 'function
                        type-sequent
                        data-sequent)
                  (ns env))))
  new-env)

(define (formal-binding? e)
  (and (list? e)
       (not (member '-> e))))

(define (formal-var? e)
  (and (symbol? e)
       (eq? ":" (substring (symbol->string e) 0 1))))

(define (equal-var? v1 v2)
  (and (equal? v1 v2)
       (eq? (cadr v1) (cadr v2))))

(define (arrow->arrow* arrow)
  (define bs '())
  (define (formal-var->var v)
    (cond []
          [else (list 1 (vector v))]))
  (define (arrow->left l)
    (cond [(eq? '-> (car l)) '()]
          [else (cons (car l)
                      (arrow->left (cdr l)))]))
  (define (arrow->right arrow)
    (cond [(eq? '-> (car l)) (cdr l)]
          [else (arrow->right (cdr l))]))
  (define (recur1 l)
    (cond [(formal-var? (car l))
           (cons (formal-var->var (car l)) )]
          ;; [(formal-binding? (car l)) ]
          [(list? (car l)) (recur1 (car l))]
          [else ()]))
  (define antecedent (recur1 (arrow->left arrow)))
  (define succedent (recur1 (arrow->right arrow)))
  (list antecedent succedent bs))

(define (run arrow env)
  (run* (arrow->arrow* arrow) env))

(define (run* arrow* env)
  )

(define (uni c env)
  ;; ac env -> env or fail
  )

;; (define (run/var var env)
;;   )

;; (define (run/name ))

(seq

 (deftype
   natural (-> type)
   zero (-> natural)
   succ (natural -> natural))

 (def/without-check add
   (natural natural -> natural)
   (:m zero -> :m)
   (:m :n succ -> :m :n recur succ))

 (def/without-check mul
   (natural natural -> natural)
   (:m zero -> zero)
   (:m :n succ -> :m :n recur :m add))

 (run (-> zero succ succ
          zero succ
          add)))

(define (check type/arrow data/arrow env)
  (check* (arrow->arrow* type/arrow)
          (arrow->arrow* data/arrow)
          env))

(define (check* type/arrow* data/arrow* env)
  ;; -> env or fail
  '())
