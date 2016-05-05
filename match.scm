;; I learn this from oleg
;; http://okmij.org/ftp/Scheme/macros.html#match-case-simple

(define-macro (match . body)
  `(match1 (quote (match . ,body)) . ,(flower-barcket/list body)))

(define-macro (match1 info e . cl)
  (let ([v (gensym "match1/call-by-value/var")])
    `(let ([,v ,e]) ;; to call by value
       (match2 ,info ,v . ,cl))))

(define-macro (match2 info v . cl)
  (cond [(null? cl)
         `(let ()
            (format #t "\n")
            (format #t "<begin-match-report>\n")
            (format #t ":value:\n")
            (display ,v) (newline)
            (format #t ":body:\n")
            (display ,info) (newline)
            (format #t "<end-match-report>\n")
            (error 'match ">_<"))]
        [else
         (let* ([v v] [c (car cl)]
                [p (car c)]
                [el (cdr c)]
                [false-body (gensym "match2/false-body/var")])
           `(let ([,false-body (lambda () (match2 ,info ,v . ,(cdr cl)))])
              ;; note that
              ;; match3 may do binding here
              ;; other clauses are outside of these binding
              (match3 ,info ,v ,p (let () . ,el) (,false-body))))]))

(define-macro (match3 info v p t f)
  ;; (: info value pattern true-body false-body -> body)
  (cond [(eq? p '__)
         t]
        [(eq? p '())
         `(if (null? ,v) ,t ,f)]
        [(and (pair? p)
              (eq? (car p) 'list))
         ;; this is for (list ...)
         ;; return by {...}
         `(match3 ,info ,v ,(cdr p) ,t ,f)]
        [(and (pair? p)
              (eq? (car p) 'quote))
         `(if (equal? ,v ,p) ,t ,f)]
        [(pair? p)
         (let ([x (car p)]
               [y (cdr p)])
           `(if (pair? ,v)
              (match3 ,info (car ,v) ,x
                      (match3 ,info (cdr ,v) ,y ,t ,f)
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
;; ;; => 3

;; (match {'b 2 3}
;;   [{'a b c}
;;    (let ([a 1])
;;      {{a b c} a {a b c} b {a b c} c {a b c}})])
;; ;; error report

;; (match {'a 2 3}
;;     [{'a b c}
;;      (let ([a 1])
;;        {{a b c} a {a b c} b {a b c} c {a b c}})])
;; ;; => ((1 2 3) 1 (1 2 3) 2 (1 2 3) 3 (1 2 3))

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
