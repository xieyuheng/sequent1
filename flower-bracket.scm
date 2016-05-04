;; {...} can not occur in the position of literal cdr
;; for example `(1 . {2}) can not be handled
;; this error will be reported by chez as
;; Exception in read: more than one item found after dot (.)

(define (flower-barcket/list l)
  ;; (: list -> parsed-list)
  (cond [(not (pair? l)) l]
        [(pair? (car l))
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
  (cond [(null? l) (error 'flower-barcket/read "sexp in lack of }")]
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
        [(pair? (car l))
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
