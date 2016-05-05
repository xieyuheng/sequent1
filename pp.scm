;; define these here to avoid confusing paren-balancers
(define lparen #\()
(define rparen #\))

;; wr is the driver, dispatching on the type of x
(define wr
  (lambda (x d? p)
    (cond
     [(symbol? x) (put-string p (symbol->string x))]
     [(pair? x) (wrpair x d? p)]
     [(number? x) (put-string p (number->string x))]
     [(null? x) (put-string p "()")]
     [(boolean? x) (put-string p (if x "#t" "#f"))]
     [(char? x) (if d? (put-char p x) (wrchar x p))]
     [(string? x) (if d? (put-string p x) (wrstring x p))]
     [(vector? x) (wrvector x d? p)]
     [(bytevector? x) (wrbytevector x d? p)]
     [(eof-object? x) (put-string p "#<eof>")]
     [(port? x) (put-string p "#<port>")]
     [(procedure? x) (put-string p "#<procedure>")]
     [else (put-string p "#<unknown>")])))

;; wrpair handles pairs and nonempty lists
(define wrpair
  (lambda (x d? p)
    (put-char p lparen)
    (let loop ([x x])
      (wr (car x) d? p)
      (cond
       [(pair? (cdr x)) (put-char p #\space) (loop (cdr x))]
       [(null? (cdr x))]
       [else (put-string p " . ") (wr (cdr x) d? p)]))
    (put-char p rparen)))

;; wrchar handles characters.  Used only when d? is #f.
(define wrchar
  (lambda (x p)
    (put-string p "#\\")
    (cond
     [(assq x '((#\alarm . "alarm") (#\backspace . "backspace")
                (#\delete . "delete") (#\esc . "esc")
                (#\newline . "newline") (#\nul . "nul")
                (#\page . "page") (#\return . "return")
                (#\space . "space") (#\tab . "tab")
                (#\vtab . "vtab"))) =>
                (lambda (a) (put-string p (cdr a)))]
     [else (put-char p x)])))

;; wrstring handles strings.  Used only when d? is #f.
(define wrstring
  (lambda (x p)
    (put-char p #\")
    (let ([n (string-length x)])
      (do ([i 0 (+ i 1)])
          ((= i n))
        (let ([c (string-ref x i)])
          (case c
            [(#\alarm) (put-string p "\\a")]
            [(#\backspace) (put-string p "\\b")]
            [(#\newline) (put-string p "\\n")]
            [(#\page) (put-string p "\\f")]
            [(#\return) (put-string p "\\r")]
            [(#\tab) (put-string p "\\t")]
            [(#\vtab) (put-string p "\\v")]
            [(#\") (put-string p "\\\"")]
            [(#\\) (put-string p "\\\\")]
            [else (put-char p c)]))))
    (put-char p #\")))

(define wrvector
  (lambda (x d? p)
    (put-char p #\#)
    (let ([n (vector-length x)])
      (do ([i 0 (+ i 1)] [sep lparen #\space])
          ((= i n))
        (put-char p sep)
        (wr (vector-ref x i) d? p)))
    (put-char p rparen)))

(define wrbytevector
  (lambda (x d? p)
    (put-string p "#vu8")
    (let ([n (bytevector-length x)])
      (do ([i 0 (+ i 1)] [sep lparen #\space])
          ((= i n))
        (put-char p sep)
        (wr (bytevector-u8-ref x i) d? p)))
    (put-char p rparen)))

;; check-and-wr is called when the port is supplied
(define check-and-wr
  (lambda (who x d? p)
    (unless (and (output-port? p) (textual-port? p))
      (assertion-violation who "invalid argument" p))
    (wr x d? p)))

;; put-datum calls wr with d? set to #f
(define put-datum
  (lambda (p x)
    (check-and-wr 'put-datum x #f p)))

;; write calls wr with d? set to #f
(define write
  (case-lambda
   [(x) (wr x #f (current-output-port))]
   [(x p) (check-and-wr 'write x #f p)]))

;; display calls wr with d? set to #t
(define display
  (case-lambda
   [(x) (wr x #t (current-output-port))]
   [(x p) (check-and-wr 'display x #t p)]))
