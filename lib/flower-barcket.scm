;; {...} can not occur in the position of literal cdr
;; for example `(1 . {2}) can not be handled
;; this error will be reported by chez as
;; Exception in read: more than one item found after dot (.)

(define (flower-barcket f l)
  ;; (: (list -> parsed-result) list -> parsed-list)
  (cond [(not (pair? l)) l]
        [(pair? (car l))
         (cons (flower-barcket f (car l))
               (flower-barcket f (cdr l)))]
        [(eq? '|{| (car l))
         (let* ([pair (flower-barcket/read f (cdr l))]
                [dl (car pair)]
                [rl (cdr pair)])
           (cons (f dl)
                 (flower-barcket f rl)))]
        [else
         (cons (car l)
               (flower-barcket f (cdr l)))]))

(define (flower-barcket/read f l)
  ;; (: (list -> parsed-result) list -> (readed-list . not-parsed-rest-list))
  (cond [(null? l) (error 'flower-barcket/read "sexp in lack of }")]
        [(eq? '|}| (car l))
         (cons '()
               (cdr l))]
        [(eq? '|{| (car l))
         (let* ([pair (flower-barcket/read f (cdr l))]
                [dl (car pair)]
                [rl (cdr pair)]
                [pair1 (flower-barcket/read f rl)]
                [dl1 (car pair1)]
                [rl1 (cdr pair1)])
           (cons (cons (f dl) dl1)
                 rl1))]
        [(pair? (car l))
         (let* ([pair (flower-barcket/read f (cdr l))]
                [dl (car pair)]
                [rl (cdr pair)])
           (cons (cons (flower-barcket f (car l)) dl)
                 rl))]
        [else
         (let* ([pair (flower-barcket/read f (cdr l))]
                [dl (car pair)]
                [rl (cdr pair)])
           (cons (cons (car l) dl)
                 rl))]))

(define (flower-barcket/list l)
  ;; (: list -> parsed-list)
  (flower-barcket (lambda (dl) (cons 'list dl)) l))

;; (flower-barcket/list
;;  '({a {a b c b {a b c} c}))
;; Exception in flower-barcket/read: sexp in lack of }

;; (flower-barcket/list
;;  '({a {a b c} } } } b {a b c} c}))
;; sadly additional |}| is not reported as error
;; => ((list a (list a b c)) |}| |}| b (list a b c) c |}|)

;; (flower-barcket/list
;;  '({a {a b c} b {a b c} c}))
;; => ((list a (list a b c) b (list a b c) c))

;; (flower-barcket/list
;;  '({{a b c} ({a b c} a {a b c} . b)} {a b c} . b))
;; => ((list (list a b c) ((list a b c) a (list a b c) . b)) (list a b c) . b)
