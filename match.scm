;; I learn this from oleg
;; http://okmij.org/ftp/Scheme/macros.html#match-case-simple

(define (flower-barcket/list l)
  ;; (: list -> parsed-list)
  (cond [(null? l) '()]
        [(list? (car l))
         (cons (flower-barcket/list (car l))
               (flower-barcket/list (cdr l)))]
        [(eq? '|{| (car l))
         (let* ([pair (flower-barcket/read (cdr l))]
                [dl (car pair)]
                [rl (cdr pair)])
           (cons (cons 'list dl)
                 (flower-barcket/list rl)))]
        [else
         (cons (car l)
               (flower-barcket/list (cdr l)))]))

(define (flower-barcket/read l)
  ;; (: list -> (readed-list . not-parsed-rest-list))
  (cond [(null? l) (error "flower-barcket/read")]
        [(eq? '|}| (car l))
         (cons '()
               (cdr l))]
        [(eq? '|{| (car l))
         (let* ([pair (flower-barcket/read (cdr l))]
                [dl (car pair)]
                [rl (cdr pair)]
                [pair1 (flower-barcket/read rl)]
                [dl1 (car pair1)]
                [rl1 (cdr pair1)])
           (cons (cons (cons 'list dl) dl1)
                 rl1))]
        [(list? (car l))
         (let* ([pair (flower-barcket/read (cdr l))]
                [dl (car pair)]
                [rl (cdr pair)])
           (cons (cons (flower-barcket/list (car l)) dl)
                 rl))]
        [else
         (let* ([pair (flower-barcket/read (cdr l))]
                [dl (car pair)]
                [rl (cdr pair)])
           (cons (cons (car l) dl)
                 rl))]))

;; (flower-barcket/list
;;  '({a {a b c} b {a b c} c}))

(define-macro (match . body)
  `(match1 . ,(flower-barcket/list body)))

(define-macro (match1 e . cl)
  (let ([v (gensym)])
    `(let ([,v ,e]) ;; to call by value
       (match2 ,v . ,cl))))

(define-macro (match2 v . cl)
  (cond [(null? cl)
         `(error 'match "failed match" ,v)]
        [else
         (let* ([v v] [c (car cl)]
                [p (car c)]
                [el (cdr c)]
                [false-body (gensym)])
           `(let ([,false-body (lambda () (match2 ,v . ,(cdr cl)))])
              ;; note that
              ;; match4 may do binding here
              ;; other clauses are outside of these binding
              (match4 ,v ,p (let () . ,el) (,false-body))))]))

(define-macro (match4 v p t f)
  ;; (: value pattern true-body false-body -> body)
  (cond [(eq? p '__)
         t]
        [(eq? p '())
         `(if (null? ,v) ,t ,f)]
        [(and (pair? p)
              (eq? (car p) 'list))
         ;; this is for (list ...)
         ;; return by {...}
         `(match4 ,v ,(cdr p) ,t ,f)]
        [(and (pair? p)
              (eq? (car p) 'quote))
         `(if (equal? ,v ,p) ,t ,f)]
        [(pair? p)
         (let ([x (car p)]
               [y (cdr p)])
           `(if (pair? ,v)
              (match4 (car ,v) ,x
                      (match4 (cdr ,v) ,y ,t ,f)
                      ,f)
              ,f))]
        [(symbol? p)
         `(let ([,p ,v]) ,t)]
        [else ;; p is literal value
         `(if (equal? ,v ,p) ,t ,f)]))

;; (let ()
;;   (define (test-match x)
;;     (match x
;;       [() "null"]
;;       [#t "bool"]
;;       [#f "bool"]
;;       [(x . y)
;;        (string-append "pair of " (test-match x) " and " (test-match y))]
;;       [__
;;        (if (symbol? x)
;;          "symbol"
;;          "something else")]))
;;   (newline)
;;   (for-each (lambda (x) (display (test-match x)) (newline))
;;             '(cctv
;;               #t
;;               "str"
;;               (#t #f "str"))))

;; (match 3
;;   [a a])

;; ;; => ((1 2 3) 1 (1 2 3) 2 (1 2 3) 3 (1 2 3))

;; (match {'a 2 3}
;;   [{'a b c}
;;    (let ([a 1])
;;      {{a b c} a {a b c} b {a b c} c {a b c}})])

;; (let ([s (list 'a 2 3)])
;;   (match s
;;     [('a b c)
;;      (let ([a 1])
;;        (list (list a b c) a (list a b c) b (list a b c) c (list a b c)))]))

;; (let ([s (list 'a 2 3)])
;;   (match s
;;     [{'a b c}
;;      (let ([a 1])
;;        {{a b c} a {a b c} b {a b c} c {a b c}})]))
