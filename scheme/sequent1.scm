;; module system of guile
;; using http://synthcode.com/scheme/match.scm
(use-modules (ice-9 match))

(define-syntax pam
  (syntax-rules ()
    [(pam v (p e ...) ...)
     (match v (p (pam-one e) ...) ...)]))

(define-syntax pam-one
  (syntax-rules (quote)
    [(_ ((quote s) b ...)) (list (quote s) (pam-one b) ...)]
    [(_ (f b ...)) (f (pam-one b) ...)]
    [(_ e) e]))

(pam '(a b c)
  [('a 'b . _) ('d ('k))])

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












