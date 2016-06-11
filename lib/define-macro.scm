;; code taken from a example in chez repo :: ChezScheme/examples/compat.ss

;;; thanks to Michael Lenaghan (MichaelL@frogware.com) for suggesting
;;; various improvements.

(define-syntax define-macro
  (lambda (x)
    (syntax-case x ()
      [(k (name arg1 ... . args)
          form1
          form2
          ...)
       #'(k name (arg1 ... . args)
            form1
            form2
            ...)]
      [(k (name arg1 arg2 ...)
          form1
          form2
          ...)
       #'(k name (arg1 arg2 ...)
            form1
            form2
            ...)]
      [(k name args . forms)
       (identifier? #'name)
       (letrec ((add-car
                 (lambda (access)
                   (case (car access)
                     ((cdr) `(cadr ,@(cdr access)))
                     ((cadr) `(caadr ,@(cdr access)))
                     ((cddr) `(caddr ,@(cdr access)))
                     ((cdddr) `(cadddr ,@(cdr access)))
                     (else `(car ,access)))))
                (add-cdr
                 (lambda (access)
                   (case (car access)
                     ((cdr) `(cddr ,@(cdr access)))
                     ((cadr) `(cdadr ,@(cdr access)))
                     ((cddr) `(cdddr ,@(cdr access)))
                     ((cdddr) `(cddddr ,@(cdr access)))
                     (else `(cdr ,access)))))
                (parse
                 (lambda (l access)
                   (cond
                    ((null? l) '())
                    ((symbol? l) `((,l ,access)))
                    ((pair? l)
                     (append!
                       (parse (car l) (add-car access))
                       (parse (cdr l) (add-cdr access))))
                    (else
                     (syntax-error #'args
                                   (format "invalid ~s parameter syntax" (datum k))))))))
         (with-syntax ((proc (datum->syntax-object #'k
                                                   (let ((g (gensym)))
                                                     `(lambda (,g)
                                                        (let ,(parse (datum args) `(cdr ,g))
                                                          ,@(datum forms)))))))
           #'(define-syntax name
               (lambda (x)
                 (syntax-case x ()
                   ((k1 . r)
                    (datum->syntax-object #'k1
                                          (proc (syntax-object->datum x)))))))))])))
