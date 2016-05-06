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
