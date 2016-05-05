(test
 (map (lambda (x) (pass1/arrow 0 x))
   '((natural natural -> natural)
     (natural natural -> (natural natural -> natural) natural)
     (:m zero -> :m)
     (:m :n succ -> :m :n recur succ)
     (:m :n succ -> :m :n (lambda (natural natural -> natural)
                            (:m :n succ -> :m :n recur succ)
                            (:m :n succ -> :m :n recur succ)))
     ((:t : type) :t -> type)
     ((:t @ type) :t -> type)
     ((:t^2 : type) :t -> type)
     ((:t1 :t2^2 :t3^0 : j k) :t -> type)
     ((:t^2 @ type) :t -> type)))
 '((((form2/name natural) (form2/name natural))
    ((form2/name natural)))
   (((form2/name natural) (form2/name natural))
    ((form2/arrow (((form2/name natural) (form2/name natural)) ((form2/name natural)))) (form2/name natural)))
   (((form2/var (:m 0)) (form2/name zero))
    ((form2/var (:m 0))))
   (((form2/var (:m 0)) (form2/var (:n 0)) (form2/name succ))
    ((form2/var (:m 0)) (form2/var (:n 0)) (form2/name recur) (form2/name succ)))
   (((form2/var (:m 0)) (form2/var (:n 0)) (form2/name succ))
    ((form2/var (:m 0)) (form2/var (:n 0)) (form2/lambda ((((form2/name natural) (form2/name natural)) ((form2/name natural))) ((((form2/var (:m 0)) (form2/var (:n 0)) (form2/name succ)) ((form2/var (:m 0)) (form2/var (:n 0)) (form2/name recur) (form2/name succ))) (((form2/var (:m 0)) (form2/var (:n 0)) (form2/name succ)) ((form2/var (:m 0)) (form2/var (:n 0)) (form2/name recur) (form2/name succ))))))))
   (((form2/bind (((form2/var (:t 1))) ((form2/name type)) #f)) (form2/var (:t 0)))
    ((form2/name type)))
   (((form2/bind (((form2/var (:t 1))) ((form2/name type)) #t)) (form2/var (:t 0)))
    ((form2/name type)))
   (((form2/bind (((form2/var (:t 2))) ((form2/name type)) #f)) (form2/var (:t 0)))
    ((form2/name type)))
   (((form2/bind (((form2/var (:t1 1)) (form2/var (:t2 2)) (form2/var (:t3 0))) ((form2/name j) (form2/name k)) #f)) (form2/var (:t 0)))
    ((form2/name type)))
   (((form2/bind (((form2/var (:t 2))) ((form2/name type)) #t)) (form2/var (:t 0)))
    ((form2/name type)))))

(test
 (map (lambda (x) (pass2/arrow x '()))
   (map (lambda (x) (pass1/arrow 0 x))
     '((natural natural -> natural)
       (natural natural -> (natural natural -> natural) natural)
       (:m zero -> :m)
       (:m :n succ -> :m :n recur succ)
       (:m :n succ -> :m :n (lambda (natural natural -> natural)
                              (:m :n succ -> :m :n recur succ)
                              (:m :n succ -> :m :n recur succ)))
       ((:t : type) :t -> type)
       ((:t @ type) :t -> type)
       ((:t^2 : type) :t -> type)
       ((:t1 :t2^2 :t3^0 : j k) :t -> type)
       ((:t^2 @ type) :t -> type))))
 '(((((form3/name natural) (form3/name natural)) ((form3/name natural))) ())
   ((((form3/name natural) (form3/name natural)) ((form3/arrow (((form3/name natural) (form3/name natural)) ((form3/name natural)))) (form3/name natural))) ())
   ((((form3/var (#(:m ()) 0)) (form3/name zero)) ((form3/var (#(:m ()) 0)))) ((:m . #(:m ()))))
   ((((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name succ))
     ((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name recur) (form3/name succ)))
    ((:n . #(:n ())) (:m . #(:m ()))))
   ((((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name succ)) ((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/lambda (((((form3/name natural) (form3/name natural)) ((form3/name natural))) ((:n . #(:n ())) (:m . #(:m ())))) (((((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name succ)) ((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name recur) (form3/name succ))) ((:n . #(:n ())) (:m . #(:m ())))) ((((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name succ)) ((form3/var (#(:m ()) 0)) (form3/var (#(:n ()) 0)) (form3/name recur) (form3/name succ))) ((:n . #(:n ())) (:m . #(:m ()))))))))) ((:n . #(:n ())) (:m . #(:m ()))))
   ((((form3/bind (((form3/var (#(:t ()) 1))) ((form3/name type)) #f)) (form3/var (#(:t ()) 0))) ((form3/name type))) ((:t . #(:t ()))))
   ((((form3/bind (((form3/var (#(:t ()) 1))) ((form3/name type)) #t)) (form3/var (#(:t ()) 0))) ((form3/name type))) ((:t . #(:t ()))))
   ((((form3/bind (((form3/var (#(:t ()) 2))) ((form3/name type)) #f)) (form3/var (#(:t ()) 0))) ((form3/name type))) ((:t . #(:t ()))))
   ((((form3/bind (((form3/var (#(:t1 ()) 1)) (form3/var (#(:t2 ()) 2)) (form3/var (#(:t3 ()) 0))) ((form3/name j) (form3/name k)) #f)) (form3/var (#(:t ()) 0))) ((form3/name type))) ((:t . #(:t ())) (:t3 . #(:t3 ())) (:t2 . #(:t2 ())) (:t1 . #(:t1 ()))))
   ((((form3/bind (((form3/var (#(:t ()) 2))) ((form3/name type)) #t)) (form3/var (#(:t ()) 0))) ((form3/name type))) ((:t . #(:t ()))))))

(eva

 (dt natural (-> type)
     zero (-> natural)
     succ (natural -> natural))

 (df add (natural natural -> natural)
     (:m zero -> :m)
     (:m :n succ -> :m :n add succ))

 (df mul (natural natural -> natural)
     (:m zero -> zero)
     (:m :n succ -> :m :n mul :m add))

 (ap (->
      zero succ
      zero succ succ
      add))

 (ap (->
      zero succ succ
      zero succ succ
      mul)))

(eva

 (dt natural (-> type)
     zero (-> natural)
     succ (natural -> natural))

 (df add (natural natural -> natural)
     (:m zero -> :m)
     (:m :n succ -> :m :n add succ))

 (df mul (natural natural -> natural)
     (:m zero -> zero)
     (:m :n succ -> :m :n mul :m add))

 (dt list ((:t : type) :t -> type)
     null (-> :t list)
     cons (:t list :t -> :t list))

 ;; (df map (:t1 list (:t1 -> :t2) -> :t2 list)
 ;;     (null :f -> null)
 ;;     (:l :e cons :f -> :e :f apply :l :f map cons))

 (df append (:t list :t list -> :t list)
     (:l null -> :l)
     (:l :r :e cons -> :l :r append :e cons))

 (ap (->
      null
      zero cons
      null
      zero cons
      append))

 (ap (->
      null
      zero cons
      zero cons
      null
      zero cons
      zero cons
      append)))

(eva

 (dt natural (-> type)
     zero (-> natural)
     succ (natural -> natural))

 (df add (natural natural -> natural)
     (:m zero -> :m)
     (:m :n succ -> :m :n add succ))

 (df mul (natural natural -> natural)
     (:m zero -> zero)
     (:m :n succ -> :m :n mul :m add))

 (dt vector ((:t : type) natural :t -> type)
     null (-> zero :t vector)
     cons (:n :t vector :t -> :n succ :t vector))

 ;; (df map (:n :t1 vector (:t1 -> :t2) -> :n :t2 vector)
 ;;     (null :f -> null)
 ;;     (:l :e cons :f -> :e :f apply :l :f map cons))

 (df append (:m :t vector :n :t vector -> :m :n add :t vector)
     (:l null -> :l)
     (:l :r :e cons -> :l :r append :e cons))

 (ap (->
      null
      zero cons
      zero cons
      zero cons
      null
      zero cons
      zero cons
      zero cons
      append)))