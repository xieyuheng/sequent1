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
    [(run) (eva/run (cdr exp) env)]))

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
  (env/ns env
          (cons (list (car body) 'function
                      (list (cadr body))
                      (cddr body))
                (ns env))))

;; (define (eva/run body env)
;;   env)

;; (define (uni c env)
;;   ;; ac env -> env or fail
;;   )

;; (define (run l env)
;;   ;; l env -> env or fail
;;   (if (eq? '() (cdr l))
;;     (cp (car l) env)
;;     (())))

;; (define (cp e env)
;;   ;; e env -> env or fail
;;   (cond [(var? e) (cp/var e env)]
;;         [(name? e) (cp/name e env)]))

;; (define (cp/var var env)
;;   )

;; (define (ch >< env)
;;   ;; env -> env
;;   )
