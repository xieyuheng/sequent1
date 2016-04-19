;; (setf eg1 '(in (list h1 h2 t1 t2)
;;             let
;;             (h1 . t1) = (foo x)
;;             (h2 . t2) = (bar y)))

(defun left-of (s l)
  (cond ((eq s (car l)) '())
        (:else (cons (car l) (left-of s (cdr l))))))

(defun right-of (s l)
  (cond ((eq s (car l)) (cdr l))
        (:else (right-of s (cdr l)))))

(defun find-char (char string &key (curser 0))
  (if (>= curser (length string))
      nil
      (let ((char0 (subseq string curser (+ 1 curser))))
        (if (equal char char0)
            curser
            (find-char char string :curser (+ 1 curser))))))

;; (cat (:to *standard-output*)
;;   ("~A" 123)
;;   ("~A" 456))
;; ==>
;; (concatenate
;;  'string
;;  (format *standard-output* "~A" 123)
;;  (format *standard-output* "~A" 456))

;; (defmacro cat
;;     ((&key (to nil))
;;      &body form/list-of-list)
;;   (let* ((form/list-of-list/2
;;           (mapcar (lambda (list) (append `(format ,to) list))
;;                   form/list-of-list))
;;          (form/final (append '(concatenate (quote string))
;;                              form/list-of-list/2)))
;;     form/final))

(defmacro cat
    ((&key (to nil)
           (trim '())
           prefix
           postfix
           letter)
     &body form/list-of-list)
  (let* ((form/list-of-list/2
          (apply (function append)
                 (mapcar (lambda (list)
                           (list prefix
                                 (list 'string-trim trim
                                       (append '(format nil) list))
                                 postfix))
                         form/list-of-list)))
         (form/list-of-list/3
          (append '(concatenate (quote string))
                  form/list-of-list/2))
         (form/final
          (cond ((equal letter :big)
                 (list 'string-upcase form/list-of-list/3))
                ((equal letter :small)
                 (list 'string-downcase form/list-of-list/3))
                ((equal letter nil)
                 form/list-of-list/3)
                (:else
                 (error "the argument :letter of (cat) must be :big or :small or nil")))))
    `(let ((string-for-return ,form/final))
       (format ,to "~A" string-for-return)
       string-for-return)))

;; (cat (:to *standard-output*
;;           :trim '(#\Space)
;;           :prefix "* "
;;           :postfix (cat () ("~%")))
;;   ("~A" "      123   ")
;;   ("~A" "   456   "))

(defmacro orz
    ((&key (to nil)
           (trim '())
           prefix
           postfix
           letter)
     &body form/list-of-list)
  `(error (cat (:to ,to
                    :trim ,trim
                    :prefix ,prefix
                    :postfix ,postfix
                    :letter ,letter)
            ,@form/list-of-list)))

(cat ()
  ("~A" 123)
  ("~A" 456))
;; ==> "123456"

;; (cat ()
;;   ("~A" 123)
;;   ("~A" 456))

;; (cat (:to *standard-output*)
;;   ("~%")
;;   ("~A~%" 123)
;;   ("~A~%" 456))

;; (let ((x 123))
;;   (cat (:to *standard-output*)
;;     ("~A~%" x)))

(defun parse/arrow (s)
  ;; sexp-arrow -> formal-arrow
  (list (parse/cedent 0 (left-of '-> s))
          (parse/cedent 0 (right-of '-> s))))

(defun parse/cedent (default-level s)
  ;; default-level, sexp-cedent -> formal-cedent
  (match s
    () => ()
    (h . r) => (cons (parse/dispatch default-level h)
                     (parse/cedent default-level r))))

(defun parse/dispatch (default-level v)
  ;; default-level, sexp-form -> formal-form
  (flet ((var? (v) (keywordp v))
         (name? (v) (and (symbolp v) (not (keywordp v))))
         (arrow? (v) (and (listp v) (member '-> v)))
         (im-bind? (v) (and (listp v) (member :> v)))
         (ex-bind? (v) (and (listp v) (member '@ v))))
    (cond ((var? v) (list 'v (parse/var default-level v)))
          ((name? v) (list 'n v))
          ((arrow? v) (list 'a (parse/arrow v)))
          ((im-bind? v) (list 'b
                              (list (parse/cedent 1 (left-of :> v))
                                    (parse/cedent 0 (right-of :> v))
                                    nil)))
          ((ex-bind? v) (list 'b
                              (list (parse/cedent 1 (left-of '@ v))
                                    (parse/cedent 0 (right-of '@ v))
                                    :true))))))

(defun parse/var (default-level v)
  ;; default-level, keyword -> formal-var
  (let* ((string (symbol-name v))
         (cursor (find-char "^" string)))
    (if cursor
        (list (intern (subseq string 0 cursor) :keyword)
              (parse-integer string
                             :start (+ 1 cursor)
                             :junk-allowed t
                             :radix 10))
        (list v default-level))))



(defun pass1/arrow (f s)
  ;; formal-arrow, scope -> arrow
  (match f
    (fac fsc) =>
    (match (pass1/cedent fac s)
      (ac s0) =>
      (match (pass1/cedent fsc s0)
        (sc s1) =>
        (list ac sc)))))

(defun pass1/cedent (f s)
  ;; formal-cedent, scope -> (cedent scope)
  (match f
    () => (list () s)
    (h . r) =>
    (match (pass1/dispatch h s)
      (v s0) =>
      (match (pass1/cedent r s0)
        (c s1) =>
        (list (cons v c) s1)))))

(defun pass1/dispatch (f s)
  ;; formal-form, scope -> (form scope)
  (match f
    ('v v) => (pass1/var v s)
    ('n n) => (list (list 'name n) s)
    ('a a) => (list (list 'arrow (pass1/arrow a s)) s)
    ('b b) => (pass1/bind b s)))

(defun pass1/var (v s)
  ;; formal-var, scope -> (var scope)
  (match v
    (symbol level) =>
    (let ((found (assoc symbol s :test #'eq)))
      (if found
          (let ((old (cdr found)))
            (list (list 'var (list old level)) s))
          (let ((new (vector symbol ())))
            (list (list 'var (list new level))
                  (cons (cons symbol new) s)))))))

(defun pass1/bind (b s)
  ;; formal-bind, scope -> (bind scope)
  (match b
    (fvs fc live?) =>
    (match (pass1/cedent fvs s)
      (vs s0) =>
      (match (pass1/cedent fc s0)
        ;; this means vars in fvs can occur in fc
        (c s1) =>
        (list (list 'bind (list vs c live?)) s1)))))


(defun env->ds (e) (car e))
(defun env->bs (e) (cadr e))
(defun env->ns (e) (caddr e))

(defun id->ls (id)
  (svref id 1))

(defun id/commit! (id ls)
  (setf (svref id 1)
        (append (svref id 1) ls)))

(defun apply/arrow (a e)
  ;; arrow, env -> env or nil
  (match e
    (ds bs ns) =>
    (match a
      (ac sc) =>
      (match (unify
              (apply/cedent
               ac
               (list (cons 'unify-point ds)
                     (cons '(commit-point) bs)
                     ns)))
        (:fail _) => nil
        (:success e1)
        => (let ((e2 (apply/cedent sc e1)))
             (match e2
               (ds2 bs2 ns2) =>
               (labels ((recur (l) ;; side-effect on var
                          (cond ((equal '(commit-point) (car l))
                                 (cdr l))
                                (:else
                                 (let* ((pair (car l))
                                        (id (car pair))
                                        (ls (cdr pair)))
                                   (id/commit! id ls)
                                   (recur (cdr l)))))))
                 (list ds2 (recur bs2) ns2))))))))

(defun apply/cedent (c e)
  ;; cedent, env -> env
  (match c
    () => e
    (h . r) => (apply/cedent r (apply/dispatch h e))))

(defun apply/dispatch (f e)
  ;; form, env -> env
  (match f
    ('var v) => (apply/var v e)
    ('name n) => (apply/name n e)
    ('arrow a) => (apply/literal-arrow a e)
    ('bind b) => (apply/bind b e)))

(defun apply/literal-arrow (a e)
  (match e
    (ds bs ns) =>
    (list (cons (list 'arrow a)
                ds)
          bs
          ns)))

(defun apply/var (v e)
  ;; var, env -> env
  (match e
    (ds bs ns) =>
    (list (cons (bs/deep bs (list 'var v)) ds)
          bs
          ns)))

(defun apply/name (n e)
  ;; name, env -> env
  (let ((found (assoc n (env->ns e) :test #'eq)))
    (if (not found)
        (orz ()
          ("apply/name unknow name : ~a~%" n))
        (let ((store (cdr found)))
          (match store
            ('function f)
            => (apply/name/function f e)
            ('type-constructor (formal-arrow arity data-name-list))
            => (apply/arity n arity e)
            ('data-constructor (formal-arrow arity type-name))
            => (apply/arity n arity e))))))

(defun apply/name/function (f e)
  ;; function, env -> env
  ;; need to do a pass1 here
  (match e
    (ds bs ns) =>
    (match f
      (formal-arrow arity formal-arrow-list) =>
      (apply/arrow-list (mapcar (lambda (x) (pass1/arrow x ()))
                                formal-arrow-list)
                        e))))

(defun apply/arrow-list (arrow-list e)
  ;; arrow-list, env -> env or nil
  (match e
    (ds bs ns) =>
    (let* ((arity (arrow-list->arity arrow-list e))
           (data-list (subseq ds 0 arity))
           (arrow-list (apply/arrow-list/filter arrow-list data-list e)))
      (match arrow-list
        () => (orz ()
                ("apply/arrow-list no match~%")
                ("  arrow-list : ~a~%" arrow-list)
                ("  data-list : ~a~%" data-list))
        (a) => (apply/arrow a e)
        (a1 a2 . _) =>
        (list (cons (list 'trunk
                          (list arrow-list
                                data-list))
                    (subseq ds arity))
              bs
              ns)))))

(defun apply/arrow-list/filter (arrow-list data-list e)
  ;; arrow-list, data-list, env -> arrow-list
  (if (eq () arrow-list)
      ()
      (match e
        (ds bs ns) =>
        (match (car arrow-list)
          (ac sc) =>
          (match (unify
                  (apply/cedent
                   ac
                   (list (cons 'unify-point
                               (append data-list ds))
                         bs
                         ns)))
            (:fail _)
            => (apply/arrow-list/filter (cdr arrow-list) data-list e)
            (:success e1)
            => (cons (car arrow-list)
                     (apply/arrow-list/filter (cdr arrow-list) data-list e)))))))

(defun apply/arity (n arity e)
  ;; name, arity, env -> env
  (match e
    (ds bs ns) =>
    (list (cons (list 'cons
                      (list n (subseq ds 0 arity)))
                (subseq ds arity))
          bs
          ns)))

(defun apply/bind (b e)
  ;; bind, env -> env
  (match b
    (vs c live?) =>
    (match (apply/cedent c e)
      ((d1 . r1) bs1 ns1) =>
      (labels ((recur (vs e)
                 (match (list vs e)
                   (() _) => e
                   ((v . r) (ds bs ns)) =>
                   (recur r (list (if live?
                                      (cons d1 ds)
                                      ds)
                                  (bs/extend 1 bs v d1)
                                  ns)))))
        (recur vs e)))))

(defun bs/find (bs v)
  ;; bs, var -> data or nil
  (match v
    (id level) =>
    (let* ((level (if (eq level nil)
                      0
                      level))
           (found/commit (assoc level (id->ls id) :test #'eq)))
      (if found/commit
          (cdr found/commit)
          (let* ((found/ls (assoc id bs :test #'eq))
                 (found/bind
                  (if found/ls
                      (assoc level (cdr found/ls) :test #'eq)
                      nil)))
            (if found/bind
                (cdr found/bind)
                nil))))))

(defun bs/walk (bs d)
  ;; bs, data -> data
  (match d
    ('var v) => (let ((found (bs/find bs v)))
                  (if found
                      (bs/walk bs found)
                      d))
    (else e) => d))

(defun bs/deep (bs d)
  ;; bs, data -> data
  (let ((d (bs/walk bs d)))
    (match d
      ('var v) => d
      ('arrow a) => d
      ('cons (name ds))
      => (list 'cons
               (list name
                     (mapcar (lambda (x) (bs/deep bs x))
                             ds)))
      ('trunk (arrow-list ds))
      => (list 'trunk
               (list arrow-list
                     (mapcar (lambda (x) (bs/deep bs x))
                             ds))))))

(defun bs/extend (default-level bs v d)
  ;; bs var data -> bs
  (match v
    (id level) =>
    (let* ((level (if (eq nil level)
                      default-level
                      level))
           (found/ls (assoc id bs :test #'eq)))
      (if found/ls
          (substitute (cons id (cons (cons level d)
                                     (cdr found/ls)))
                      (lambda (pair) (eq (car pair) id))
                      bs)
          (cons (cons id (list (cons level d)))
                bs)))))

(defun unify (e)
  ;; env -> unify-report
  (match e
    (ds bs ns) =>
    (let* ((l1 (left-of 'unify-point ds))
           (tmp (right-of 'unify-point ds))
           (len (length l1))
           (l2 (subseq tmp 0 len))
           (ds1 (subseq tmp len)))
      (unify/list l1 l2
                  (list :success (list ds1 bs ns))))))

(defun unify/list (l1 l2 unify-report)
  ;; data list, data list, unify-report -> unify-report
  (match unify-report
    (:fail report) => unify-report
    (:success e) =>
    (if (eq () l1)
        unify-report
        (unify/list (cdr l1) (cdr l2)
                    (unify/dispatch (car l1) (car l2) e)))))

(defun var/eq (v1 v2)
  (match (list v1 v2)
    ((id1 level1) (id2 level2)) =>
    (and (eq id1 id2)
         (eq level1 level2))))

(defun unify/dispatch (d1 d2 e)
  ;; data, data, env -> unify-report
  (match e
    (ds bs ns) =>
    (let ((d1 (bs/walk bs d1))
          (d2 (bs/walk bs d2)))
      ;; walk then if it is var it will be fresh
      (match (list d1 d2)
        (('var v1) ('var v2))
        => (if (var/eq v1 v2)
               (list :success e)
               (list :success
                     (list ds (bs/extend 0 bs v1 d2) ns)))
        (('var v) d)
        => (list :success
                 (list ds (bs/extend 0 bs v d) ns))
        (d ('var v))
        => (list :success
                 (list ds (bs/extend 0 bs v d) ns))
        (('arrow a1) ('arrow a2))
        => (if (equal a1 a2)
               (list :success
                     (list ds bs ns))
               (list :fail
                     (list
                      `(unify/dispatch (:d1 ,d1)
                                       (:d2 ,d2)))))
        (('arrow a) _)
        => (list :fail
                 (list
                  `(unify/dispatch (:d1 ,d1)
                                   (:d2 ,d2))))
        (_ ('arrow a))
        => (list :fail
                 (list
                  `(unify/dispatch (:d1 ,d1)
                                   (:d2 ,d2))))
        (('cons (name1 data-list1)) ('cons (name2 data-list2)))
        => (if (eq name1 name2)
               (unify/list data-list1 data-list2 (list :success e))
               (list :fail
                     (list
                      `(unify/dispatch (:d1 ,d1)
                                       (:d2 ,d2)))))
        (('trunk trunk1) ('trunk trunk2)) => (unify/trunk/trunk trunk1 trunk2 e)
        (d ('trunk trunk)) => (unify/trunk/data trunk d e)
        (('trunk trunk) d) => (unify/trunk/data trunk d e)))))

(defun unify/trunk/trunk (trunk1 trunk2 e)
  ;; trunk, trunk, env -> unify-report
  (cat () ("here unify/trunk/trunk ~%"))
  (match (list trunk1 trunk2 e)
    ((arrow-list1 data-list1) (arrow-list2 data-list2) (ds bs ns)) =>
    (if (equalp arrow-list1 arrow-list2)
        ;; the use of equalp is not safe
        (unify/list data-list1 data-list2 (list :success e))
        (match (unify/trunk/data trunk1 (list 'trunk trunk2) e)
          (:success e1) => (list :success e1)
          (:fail _) =>
          (unify/trunk/data trunk2 (list 'trunk trunk1) e)))))

(defun unify/trunk/data (trunk d e)
  ;; trunk, data, env -> unify-report
  ;; where data is not trunk
  (cat () ("here unify/trunk/data ~%"))
  (match e
    (ds bs ns) =>
    (match trunk
      (arrow-list data-list) =>
      (let ((data-list1 (mapcar (lambda (x) (bs/deep bs x))
                                data-list)))
        (match (apply/arrow-list/filter arrow-list data-list1 e)
          ()
          => (list :fail
                   (list
                    `(unify/dispatch
                      (:trunk-filter-to ())
                      (:trunk ,trunk)
                      (:data ,d))))
          (a)
          => (match (apply/arrow a (list data-list1 bs ns))
               ((h . _) bs1 ns1)
               => (unify/dispatch d h (list ds bs1 ns1)))
          (a1 a2 . _)
          => (list :fail
                   (list
                    `(unify/dispatch
                      (:trunk-filter-to
                       (:arrow-list
                        ,(apply/arrow-list/filter arrow-list data-list1 e))
                       (:data-list1 ,data-list1)
                       (:old-data-list ,data-list))
                      (:trunk ,trunk)
                      (:data ,d)))))))))

(defun eva (l e)
  ;; sexp-top list, env -> env
  (match l
    () => e
    (h . r) => (eva r (eva/top (parse/top h) e))))

(defun parse/top (s)
  ;; sexp-top -> top
  (match s
    ('dt name sexp-arrow . body)
    => (list 'dt
             (list (list name (parse/arrow sexp-arrow))
                   (parse/top/dt-body body)))
    ('df name sexp-arrow . sexp-arrow-list)
    => (list 'df
             (list (list name (parse/arrow sexp-arrow))
                   (mapcar #'parse/arrow sexp-arrow-list)))
    ('ap sexp-arrow)
    => (list 'ap (parse/arrow sexp-arrow))))

(defun parse/top/dt-body (body)
  ;; sexp-top-dt-body -> ((formal-name formal-arrow) ...)
  (cond ((eq () body) ())
        ((eq () (cdr body))
         (orz ()
           ("parse/top/dt-body wrong body : body")))
        (:else
         (cons (list (car body) (parse/arrow (cadr body)))
               (parse/top/dt-body (cddr body))))))



(defun eva/top (top e)
  ;; top, env -> env
  (match top
    ('dt type-definition) => (eva/dt type-definition e)
    ('df function-definition) => (eva/df function-definition e)
    ('ap formal-arrow) => (apply/arrow (pass1/arrow formal-arrow ()) e)))

(defun eva/dt (type-definition e)
  ;; type-definition -> env
  (match e
    (ds bs ns) =>
    (match type-definition
      ((n a) l) =>
      (let* ((name-list
              (mapcar #'car l))
             (arity
              (formal-arrow->arity a e))
             (ns1
              (cons (cons n
                          (list 'type-constructor
                                (list a
                                      arity
                                      name-list)))
                    ns)))
        (eva/dt/data-constructor-list n l (list ds bs ns1))))))

(defun eva/dt/data-constructor (type-name data-constructor e)
  ;; type-name, data-constructor, env -> env
  (match e
    (ds bs ns) =>
    (match data-constructor
      (n a) =>
      (list ds
            bs
            (cons (cons n
                        (list 'data-constructor
                              (list a
                                    (formal-arrow->arity a e)
                                    type-name)))
                  ns)))))

(defun eva/dt/data-constructor-list (type-name l e)
  ;; type-name, data-constructor-list, env -> env
  (match l
    () => e
    (h . r) =>
    (eva/dt/data-constructor-list
     type-name r
     (eva/dt/data-constructor type-name h e))))

(defun formal-arrow->arity (formal-arrow e)
  ;; formal-arrow, env -> arity
  (match e
    (ds bs ns) =>
    (arrow->arity (pass1/arrow formal-arrow ()) e)))

(defun arrow->arity (a e)
  ;; arrow, env -> arity
  (match e
    (ds bs ns) =>
    (match a
      (antecedent succedent) =>
      (match (apply/cedent antecedent
                           (list () bs ns))
        (ds1 bs1 ns1) =>
        (length ds1)))))

(defun arrow-list->arity (l e)
  ;; arrow-list, env -> arity
  (match l
    (h . _) => (arrow->arity h e)))

(defun eva/df (function-definition e)
  ;; function-definition -> env
  (match e
    (ds bs ns) =>
    (match function-definition
      ((n a) l) =>
      (let ((ns1 (cons (cons n
                             (list 'function
                                   (list a
                                         (formal-arrow->arity a e)
                                         l)))
                       ns)))
        (match (check a l (list ds bs ns1))
          ;; note that the bs of the env
          ;; returned by check is not clean
          (:success e1) => (list ds bs ns1)
          (:fail check-report) =>
          (orz ()
            ("eva/df fail to define : ~a~%" function-definition)
            ("check-report : ~a" check-report)))))))

(defun check (type-formal-arrow l e)
  ;; type-formal-arrow, formal-arrow list, env -> check-report
  (match l
    () => (list :success e)
    (h . r) =>
    (match (check/arrow type-formal-arrow h e)
      (:success e) => (check type-formal-arrow r e)
      (:fail check-report) => (list :fail check-report))))

(defun check/arrow (type-formal-arrow a e)
  ;; type-formal-arrow, formal-arrow, env -> check-report
  (match (pass1/arrow type-formal-arrow ())
    (tac tsc) =>
    (match (apply/cedent tac e)
      (ds0 bs0 ns0) =>
      (match (pass1/arrow a ())
        (ac sc) =>
        (match (unify
                (type-apply/cedent
                 ac
                 (list (cons 'unify-point ds0)
                       bs0
                       ns0)))
          (:fail report)
          => (list :fail
                   (cons `(check/arrow
                           (:type-antecedent ,tac)
                           (:antecedent ,ac))
                         report))
          (:success e1)
          => (let* ((e2 (type-apply/cedent sc e1)))
               (match e2
                 (ds2 bs2 ns2) =>
                 (match (unify
                         (apply/cedent
                          tsc
                          (list (cons 'unify-point ds2)
                                bs2
                                ns2)))
                   (:success e) => (list :success e)
                   (:fail report)
                   => (list :fail
                            (cons `(check/arrow
                                    (:type-succedent ,tsc)
                                    (:succedent ,sc))
                                  report))))))))))

(defun type-apply/cedent (c e)
  ;; cedent, env -> env
  (match c
    () => e
    (h . r) => (type-apply/cedent r (type-apply/dispatch h e))))

(defun type-apply/dispatch (f e)
  ;; form, env -> env
  (match f
    ('var v) => (type-apply/var v e)
    ('name n) => (type-apply/name n e)
    ('arrow a) => ;; (type-apply/literal-arrow a e)
    (orz ()
      ("type-apply/dispatch can not type-apply literal-arrow for now"))
    ('bind b) => ;; (type-apply/bind b e)
    (orz ()
      ("type-apply/dispatch can not type-apply bind for now"))))

(defun type-apply/var (v e)
  ;; var, env -> env
  (match v
    (id level) =>
    (apply/var (if (eq level nil)
                   (list id 1)
                   (list id (+ 1 level)))
               e)))

(defun type-apply/name (n e)
  ;; name, env -> env
  (let ((found (assoc n (env->ns e) :test #'eq)))
    (if (not found)
        (orz ()
          ("type-apply/name unknow name : ~a~%" n))
        (let ((store (cdr found)))
          (match store
            (any-store (formal-arrow arity . _)) =>
            (apply/arrow (pass1/arrow formal-arrow ()) e))))))

(defmacro sequent (&body body)
  `(eva (quote ,body)
       '(() () ())))
