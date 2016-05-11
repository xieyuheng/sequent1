(eva

 (deftype natural (-> type)
   zero (-> natural)
   succ (natural -> natural))

 (defn add (natural natural -> natural)
   (:m zero -> :m)
   (:m :n succ -> :m :n add succ))

 (defn mul (natural natural -> natural)
   (:m zero -> zero)
   (:m :n succ -> :m :n mul :m add))

 (app (->
       zero succ
       zero succ succ
       add))

 (app (->
       zero succ succ
       zero succ succ
       mul)))

(eva

 (deftype natural (-> type)
   zero (-> natural)
   succ (natural -> natural))

 (defn add (natural natural -> natural)
   (:m zero -> :m)
   (:m :n succ -> :m :n add succ))

 (defn mul (natural natural -> natural)
   (:m zero -> zero)
   (:m :n succ -> :m :n mul :m add))

 (deftype list ((:t : type) :t -> type)
   null (-> :t list)
   cons (:t list :t -> :t list))

 (defn append (:t list :t list -> :t list)
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

 (deftype natural (-> type)
   zero (-> natural)
   succ (natural -> natural))

 (defn add (natural natural -> natural)
   (:m zero -> :m)
   (:m :n succ -> :m :n add succ))

 (defn mul (natural natural -> natural)
   (:m zero -> zero)
   (:m :n succ -> :m :n mul :m add))

 (deftype vector ((:t : type) natural :t -> type)
   null (-> zero :t vector)
   cons (:n :t vector :t -> :n succ :t vector))

 (defn append (:m :t vector :n :t vector -> :m :n add :t vector)
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

 (deftype natural (-> type)
   zero (-> natural)
   succ (natural -> natural))

 (defn add (natural natural -> natural)
   (:m zero -> :m)
   (:m :n succ -> :m :n add succ))

 (defn mul (natural natural -> natural)
   (:m zero -> zero)
   (:m :n succ -> :m :n mul :m add))

 (deftype vector ((:t : type) natural :t -> type)
   null (-> zero :t vector)
   cons (:n :t vector :t -> :n succ :t vector))

 (defn append (:m :t vector :n :t vector -> :m :n add :t vector)
   (:l null -> :l)
   (:l :r :e cons -> :l :r append :e cons))

 (defn map (:n :t1 vector (:t1 -> :t2) -> :n :t2 vector)
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
