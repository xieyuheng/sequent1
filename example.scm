(eva

 (+ natural (-> type)
    zero (-> natural)
    succ (natural -> natural))

 (~ add (natural natural -> natural)
    (:m zero -> :m)
    (:m :n succ -> :m :n add succ))

 (~ mul (natural natural -> natural)
    (:m zero -> zero)
    (:m :n succ -> :m :n mul :m add))

 (~ factorial (natural -> natural)
    (zero -> zero succ)
    (:n succ -> :n factorial :n succ mul))

 (app (->
       zero succ
       zero succ succ
       add))

 (app (->
       zero succ succ
       zero succ succ
       mul))

 (app (->
       zero succ succ succ
       factorial)))

(eva

 (+ natural (-> type)
    zero (-> natural)
    succ (natural -> natural))

 (~ add (natural natural -> natural)
    (:m zero -> :m)
    (zero :m -> :m)
    (:m succ :n succ -> :m :n add succ succ))

 (+ eq ({:t : type} :t :t -> type)
    refl ({:t : type} {:d : :t} -> :d :d eq))

 (~ eq/test
    (-> zero zero add zero zero add eq)
    (-> refl))

 (~ add/commute ((:m :n : natural) -> :m :n add :n :m add eq)
    (:m zero -> refl)
    (:m :n succ -> refl)))

(eva

 (+ natural (-> type)
    zero (-> natural)
    succ (natural -> natural))

 (~ drop (:t ->)
    (:d ->))

 (~ dup (:t -> :t :t)
    (:d -> :d :d))

 (~ over (:t1 :t2 -> :t1 :t2 :t1)
    (:d1 :d2 -> :d1 :d2 :d1))

 (~ tuck (:t1 :t2 -> :t2 :t1 :t2)
    (:d1 :d2 -> :d2 :d1 :d2))

 (~ swap (:t1 :t2 -> :t2 :t1)
    (:d1 :d2 -> :d2 :d1))

 (app (-> zero
          zero succ
          swap
          drop
          dup)))

(eva

 (+ natural (-> type)
    zero (-> natural)
    succ (natural -> natural))

 (~ add (natural natural -> natural)
    (:m zero -> :m)
    (:m :n succ -> :m :n add succ))

 (~ mul (natural natural -> natural)
    (:m zero -> zero)
    (:m :n succ -> :m :n mul :m add))

 (+ list ({:t : type} :t -> type)
    null (-> :t list)
    cons (:t list :t -> :t list))

 (~ append (:t list :t list -> :t list)
    (:l null -> :l)
    (:l :r :e cons -> :l :r append :e cons))

 (app (->
       null
       zero cons
       null
       zero cons
       append))

 (app (->
       null
       zero cons
       zero cons
       null
       zero cons
       zero cons
       append)))

(eva

 (+ natural (-> type)
    zero (-> natural)
    succ (natural -> natural))

 (~ add (natural natural -> natural)
    (:m zero -> :m)
    (:m :n succ -> :m :n add succ))

 (~ mul (natural natural -> natural)
    (:m zero -> zero)
    (:m :n succ -> :m :n mul :m add))

 (+ list ({:t : type} :t -> type)
    null (-> :t list)
    cons (:t list :t -> :t list))

 (~ append (:t list :t list -> :t list)
    (:l null -> :l)
    (:l :r :e cons -> :l :r append :e cons))

 (~ map (:t1 list (:t1 -> :t2) -> :t2 list)
    (null :f -> null)
    (:l :e cons :f -> :l :f map :e :f apply cons))

 (app (->
       null
       zero cons
       zero cons
       zero cons
       null
       zero cons
       zero cons
       zero cons
       append
       (zero -> zero succ)
       map))

 (app (->
       null
       zero cons
       zero cons
       (lambda (natural -> natural)
         (zero -> zero succ))
       map))

 (+ has-length ({:t : type} :t list natural -> type)
    null/has-length (-> null zero has-length)
    cons/has-length (:l :n has-length -> :l :a cons :n succ has-length))

 (~ map/has-length (:l :n has-length -> :l :f map :n has-length)
    (null/has-length -> null/has-length)
    (:h cons/has-length -> :h map/has-length cons/has-length)))

(eva

 (+ natural (-> type)
    zero (-> natural)
    succ (natural -> natural))

 (~ add (natural natural -> natural)
    (:m zero -> :m)
    (:m :n succ -> :m :n add succ))

 ;; ;; this can not be used to prove append
 ;; (~ add (natural natural -> natural)
 ;;    (:m zero -> :m)
 ;;    (zero :m -> :m)
 ;;    (:m succ :n succ -> :m :n add succ succ))

 ;; ;; this can be used to prove append
 ;; (~ add (natural natural -> natural)
 ;;    (:m zero -> :m)
 ;;    (zero :m -> :m)
 ;;    (:m succ :n succ -> :m :n add succ succ)
 ;;    (:m :n succ -> :m :n add succ)
 ;;    (:m succ :n -> :m :n add succ))

 (~ mul (natural natural -> natural)
    (:m zero -> zero)
    (:m :n succ -> :m :n mul :m add))

 (+ vector ({:t : type} natural :t -> type)
    null (-> zero :t vector)
    cons (:n :t vector :t -> :n succ :t vector))

 (~ append (:m :t vector :n :t vector -> :m :n add :t vector)
    (:l null -> :l)
    (:l :r :e cons -> :l :r append :e cons))

 (app (->
       null
       zero cons
       zero cons
       zero cons
       null
       zero cons
       zero cons
       zero cons
       append)))

(eva

 (+ natural (-> type)
    zero (-> natural)
    succ (natural -> natural))

 (~ add (natural natural -> natural)
    (:m zero -> :m)
    (:m :n succ -> :m :n add succ))

 (~ mul (natural natural -> natural)
    (:m zero -> zero)
    (:m :n succ -> :m :n mul :m add))

 (+ vector ({:t : type} natural :t -> type)
    null (-> zero :t vector)
    cons (:n :t vector :t -> :n succ :t vector))

 (~ append (:m :t vector :n :t vector -> :m :n add :t vector)
    (:l null -> :l)
    (:l :r :e cons -> :l :r append :e cons))

 (~ map (:n :t1 vector (:t1 -> :t2) -> :n :t2 vector)
    (null :f -> null)
    (:l :e cons :f -> :l :f map :e :f apply cons))

 (app (->
       null
       zero cons
       zero cons
       zero cons
       null
       zero cons
       zero cons
       zero cons
       append
       (zero -> zero succ)
       map))

 (app (->
       null
       zero cons
       zero cons
       (lambda (natural -> natural)
         (zero -> zero succ))
       map)))

(eva

 (+ natural (-> type)
    zero (-> natural)
    succ (natural -> natural))

 (~ add (natural natural -> natural)
    (:m zero -> :m)
    (:m :n succ -> :m :n add succ))

 (~ mul (natural natural -> natural)
    (:m zero -> zero)
    (:m :n succ -> :m :n mul :m add))

 (~ natural-induction

    ((:p : (natural -> type))
     zero :p apply
     ((:k : natural) :k :p apply -> :k succ :p apply)
     (:x : natural) -> :x :p apply)

    (:p :p/z :p/s zero -> :p/z)
    (:p :p/z :p/s :k succ ->
        :k
        :p :p/z :p/s :k natural-induction
        :p/s apply)))
