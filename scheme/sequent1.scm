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
