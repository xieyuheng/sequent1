(define-syntax ->
  (syntax-rules ()
    [(_ a)
     a]
    [(_ a (f1 a1 ...))
     (f1 a a1 ...)]
    [(_ a (f1 a1 ...) b ...)
     (-> (f1 a a1 ...) b ...)]))
