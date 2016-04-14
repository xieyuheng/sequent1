(defmacro match (expression &rest patterns)
  (let* ((do-let (not (atom expression)))
         (key    (if do-let (gensym) expression))
         (cbody  (expand-select-patterns key patterns))
         (cform  `(cond . ,cbody)))
    (if do-let
        `(let ((,key ,expression)) ,cform)
        cform)))

(defun expand-select-patterns (key patterns)
  (if (eq (second patterns) '=>)
      (expand-select-patterns-style-2 key patterns)
      (expand-select-patterns-style-1 key patterns)))

(defun expand-select-patterns-style-1 (key patterns)
  (if (null patterns)
      `((T (error "Case select pattern match failure on ~S" ,key)))
      (let ((pattern  (caar patterns))
            (actions  (cdar patterns))
            (rest     (cdr patterns)) )
        (let  ((test       (compile-select-test key pattern))
               (bindings   (compile-select-bindings key pattern actions)))
          `(,(if bindings  `(,test (let ,bindings . ,actions))
                 `(,test . ,actions))
             . ,(if (eq test t)
                    nil
                    (expand-select-patterns-style-1 key rest)))))))

(defun expand-select-patterns-style-2 (key patterns)
  (if (null patterns)
      `((T (error "Case select pattern match failure on ~S" ,key)))
      (let ((pattern  (first patterns))
            (arrow    (if (or (< (length patterns) 3)
                              (not (eq (second patterns) '=>)))
                          (error "Illegal patterns: ~S" patterns)))
            (actions  (list (third patterns)))
            (rest     (cdddr patterns)))
        (let  ((test       (compile-select-test key pattern))
               (bindings   (compile-select-bindings key pattern actions)))
          `(,(if bindings  `(,test (let ,bindings . ,actions))
                 `(,test . ,actions))
             . ,(if (eq test t)
                    nil
                    (expand-select-patterns-style-2 key rest)))))))


(defun compile-select-test (key pattern)
  (let  ((tests (remove-if
                 #'(lambda (item) (eq item t))
                 (compile-select-tests key pattern))))
    (cond
      ;; note AND does this anyway, but this allows us to tell if
      ;; the pattern will always match.
      ((null tests)           t)
      ((= (length tests) 1)   (car tests))
      (T                      `(and . ,tests)))))


(defun compile-select-tests (key pattern)
  (cond ((constantp pattern)
         `((,(cond ((numberp pattern) 'eql)
                   ((symbolp pattern) 'eq)
                   (T                'equal))
             ,key ,pattern)))

        ((symbolp pattern) '(T))
        ((select-double-match? pattern)
         (append
          (compile-select-tests key (first pattern))
          (compile-select-tests key (third pattern))))
        ((select-predicate? pattern)
         (append
          `((,(second (first pattern)) ,key))
          (compile-select-tests key (second pattern))))
        ((consp pattern)
         (append
          `((consp ,key))
          (compile-select-tests (!cs-car key) (car
                                               pattern))
          (compile-select-tests (!cs-cdr key) (cdr
                                               pattern))))
        ('T         (error "Illegal select pattern: ~S" pattern))))

(defun compile-select-bindings (key pattern action)
  (cond ((constantp pattern) '())
        ((symbolp pattern)
         (if (select!-in-tree pattern action) `((,pattern ,key))
             '()))
        ((select-double-match? pattern)
         (append
          (compile-select-bindings key (first pattern) action)
          (compile-select-bindings key (third pattern)
                                   action)))
        ((select-predicate? pattern)
         (compile-select-bindings key (second pattern)
                                  action))
        ((consp pattern)
         (append
          (compile-select-bindings (!cs-car key) (car pattern)
                                   action)
          (compile-select-bindings (!cs-cdr key) (cdr pattern)
                                   action)))))

(defun select!-in-tree (atom tree)
  (or (eq atom tree)
      (if (consp tree)
          (or (select!-in-tree atom (car tree))
              (select!-in-tree atom (cdr tree))))))

(defun select-double-match? (pattern)
  ;;  (<pattern> = <pattern>)
  (and (consp pattern) (consp (cdr pattern)) (consp (cddr pattern))
       (null (cdddr pattern))
       (eq (second pattern) '=)))

(defun select-predicate? (pattern)
  ;; ((function <f>) <pattern>)
  (and    (consp pattern)
          (consp (cdr pattern))
          (null (cddr pattern))
          (consp (first pattern))
          (consp (cdr (first pattern)))
          (null (cddr (first pattern)))
          (eq (caar pattern) 'function)))

(defun !cs-car (exp)
  (!cs-car/cdr
   'car exp
   '((car . caar)    (cdr . cadr)    (caar . caaar)  (cadr . caadr)
     (cdar . cadar)  (cddr . caddr)
     (caaar . caaaar)    (caadr . caaadr)    (cadar . caadar)
     (caddr . caaddr)    (cdaar . cadaar)    (cdadr . cadadr)
     (cddar . caddar)    (cdddr . cadddr))))

(defun !cs-cdr (exp)
  (!cs-car/cdr
   'cdr exp
   '((car . cdar)    (cdr . cddr)    (caar . cdaar)  (cadr . cdadr)
     (cdar . cddar)  (cddr . cdddr)
     (caaar . cdaaar)    (caadr . cdaadr)    (cadar . cdadar)
     (caddr . cdaddr)    (cdaar . cddaar)    (cdadr . cddadr)
     (cddar . cdddar)    (cdddr . cddddr))))

(defun !cs-car/cdr (op exp table)
  (if (and (consp exp) (= (length exp) 2))
      (let ((replacement  (assoc (car exp) table)))
        (if replacement
            `(,(cdr replacement) ,(second exp))
            `(,op ,exp)))
      `(,op ,exp)))

;; (setf c1 '(match x (a 1) (b 2 3 4)))
;; (setf c2 '(match (car y)
;;            (1 (print 100) 101) (2 200) ("hello" 5) (:x 20) (else (1+
;;                                                                   else))))
;; (setf c3 '(match (caddr y)
;;            ((all = (x y)) (list x y all))
;;            ((a '= b)      (list 'assign a b))
;;            ((#'oddp k)     (1+ k))))

;;
;;  IN macro
;;
;;  (IN exp LET pat1 = exp1
;;              pat2 = exp2
;;              ...)
;;
;;  (IN exp LET* pat1 = exp1
;;               pat2 = exp2
;;               ...)
;;

(defmacro in (&rest form)
  (match form
    (exp 'let . pats) =>
    (let* ((exps   (select-in-let-parts pats 'exp))
           (pats   (select-in-let-parts pats 'pat))
           (vars   (mapcar #'(lambda (x) (gensym)) exps)))
      `(let ,(mapcar #'list vars exps)
         ,(reduce
           #'(lambda (var-pat subselection)
               (let ((var  (first var-pat))
                     (pat  (second var-pat)))
                 `(match ,var
                    ,pat => ,subselection
                    else => (error "IN-LET type error: ~S
doesnt match ~S" ,var ',pat))))
           (mapcar #'list vars pats)
           :from-end t
           :initial-value exp)))
    (exp 'let*)         => exp
    (exp 'let* pat '= patexp . pats)  =>
    (let ((var (gensym)))
      `(let ((,var ,patexp))
         (match ,var
           ,pat => (in ,exp let* . ,pats)
           else => (error "IN-LET type error: ~S doesnt match
~S" ,var ',pat))))
    else                =>
    (error "Illegal IN form ~S" form)))

(defun select-in-let-parts (pats part)
  (match pats
    nil => nil
    (pat '= exp . rest) =>
    (cons (match part
            'exp => exp
            'pat => pat)
          (select-in-let-parts rest part))
    other =>
    (error "Illegal LET form(s): ~S" pats)))

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

(assert
 (equal

  (list
   (parse/arrow '(natural natural -> natural))
   (parse/arrow '(natural natural -> (natural natural -> natural) natural))
   (parse/arrow '(:m zero -> :m))
   (parse/arrow '(:m :n succ -> :m :n recur succ))

   (parse/arrow '((:t :> type) :t -> type))

   (parse/arrow '((:t @ type) :t -> type))
   (parse/arrow '((:t^2 :> type) :t -> type))
   (parse/arrow '((:t1 :t2^2 :t3^0 :> j k) :t -> type))
   (parse/arrow '((:t^2 @ type) :t -> type)))



  '((((n natural) (n natural)) ((n natural)))
    (((n natural) (n natural)) ((a (((n natural) (n natural)) ((n natural)))) (n natural)))
    (((v (:m 0)) (n zero)) ((v (:m 0))))
    (((v (:m 0)) (v (:n 0)) (n succ)) ((v (:m 0)) (v (:n 0)) (n recur) (n succ)))

    (((b (((v (:t 1))) ((n type)) nil)) (v (:t 0))) ((n type)))

    (((b (((v (:t 1))) ((n type)) :true)) (v (:t 0))) ((n type)))
    (((b (((v (:t 2))) ((n type)) nil)) (v (:t 0))) ((n type)))
    (((b (((v (:t1 1)) (v (:t2 2)) (v (:t3 0))) ((n j) (n k)) nil)) (v (:t 0))) ((n type)))
    (((b (((v (:t 2))) ((n type)) :true)) (v (:t 0))) ((n type))))))

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

(assert
 (equalp

  (list
   (pass1/arrow
    (parse/arrow '(natural natural -> natural))
    ())
   (pass1/arrow
    (parse/arrow '(natural natural -> (natural natural -> natural) natural))
    ())
   (pass1/arrow
    (parse/arrow '(:m zero -> :m))
    ())
   (pass1/arrow
    (parse/arrow '(:m :n succ -> :m :n recur succ))
    ())
   (pass1/arrow
    (parse/arrow '((:t :> type) :t -> type))
    ())
   (pass1/arrow
    (parse/arrow '((:t @ type) :t -> type))
    ())
   (pass1/arrow
    (parse/arrow '((:t^2 :> type) :t -> type))
    ())
   (pass1/arrow
    (parse/arrow '((:t1 :t2^2 :t3^0 :> j k) :t -> type))
    ())
   (pass1/arrow
    (parse/arrow '((:t^2 @ type) :t -> type))
    ())
   (pass1/arrow
    (parse/arrow '(:t (:t -> :t) -> (:t -> (:t -> :t) :t) type))
    ()))

  '((((name natural) (name natural)) ((name natural)))
    (((name natural) (name natural)) ((arrow (((name natural) (name natural)) ((name natural)))) (name natural)))
    (((var (#(:m nil) 0)) (name zero)) ((var (#(:m nil) 0))))
    (((var (#(:m nil) 0)) (var (#(:n nil) 0)) (name succ)) ((var (#(:m nil) 0)) (var (#(:n nil) 0)) (name recur) (name succ)))
    (((bind (((var (#(:t nil) 1))) ((name type)) nil)) (var (#(:t nil) 0))) ((name type)))
    (((bind (((var (#(:t nil) 1))) ((name type)) :true)) (var (#(:t nil) 0))) ((name type)))
    (((bind (((var (#(:t nil) 2))) ((name type)) nil)) (var (#(:t nil) 0))) ((name type)))
    (((bind (((var (#(:t1 nil) 1)) (var (#(:t2 nil) 2)) (var (#(:t3 nil) 0))) ((name j) (name k)) nil)) (var (#(:t nil) 0))) ((name type)))
    (((bind (((var (#(:t nil) 2))) ((name type)) :true)) (var (#(:t nil) 0))) ((name type)))
    (((var (#(:t nil) 0)) (arrow (((var (#(:t nil) 0))) ((var (#(:t nil) 0)))))) ((arrow (((var (#(:t nil) 0))) ((arrow (((var (#(:t nil) 0))) ((var (#(:t nil) 0))))) (var (#(:t nil) 0))))) (name type))))))

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
      (let ((e1 (unify
                 (apply/cedent
                  ac
                  (list (cons 'unify-point ds)
                        (cons '(commit-point) bs)
                        ns)))))
        (if (not e1)
            nil
            (let ((e2 (apply/cedent sc e1)))
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
                  (list ds2 (recur bs2) ns2)))))))))

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
          (let ((e1 (unify
                     (apply/cedent
                      ac
                      (list (cons 'unify-point
                                  (append data-list ds))
                            bs
                            ns)))))
            (if (not e1)
                (apply/arrow-list/filter (cdr arrow-list) data-list e)
                (cons (car arrow-list)
                      (apply/arrow-list/filter (cdr arrow-list) data-list e))))))))

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
  ;; env -> env of nil
  (match e
    (ds bs ns) =>
    (let* ((l1 (left-of 'unify-point ds))
           (tmp (right-of 'unify-point ds))
           (len (length l1))
           (l2 (subseq tmp 0 len))
           (ds1 (subseq tmp len)))
      (unify/list l1 l2 (list ds1 bs ns)))))

(defun unify/list (l1 l2 e)
  ;; data list, data list, env => env or nil
  (cond ((eq nil e) nil)
        ((eq () l1) e)
        (:else
         (unify/list (cdr l1) (cdr l2)
                     (unify/one (car l1) (car l2) e)))))

(defun var/eq (v1 v2)
  (match (list v1 v2)
    ((id1 level1) (id2 level2)) =>
    (and (eq id1 id2)
         (eq level1 level2))))

(defun unify/one (d1 d2 e)
  ;; data, data, env => env or nil
  (match e
    (ds bs ns) =>
    (let ((d1 (bs/walk bs d1))
          (d2 (bs/walk bs d2)))
      ;; walk then if it is var it will be fresh
      (match (list d1 d2)
        (('var v1) ('var v2))
        => (if (var/eq v1 v2)
               e
               (list ds (bs/extend 0 bs v1 d2) ns))
        (('var v) d) => (list ds (bs/extend 0 bs v d) ns)
        (d ('var v)) => (list ds (bs/extend 0 bs v d) ns)
        (('arrow a1) ('arrow a2))
        => (if (equal a1 a2)
               (list ds bs ns)
               nil)
        (('arrow a) _) => nil
        (_ ('arrow a)) => nil
        (('cons (name1 data-list1)) ('cons (name2 data-list2)))
        => (if (eq name1 name2)
               (unify/list data-list1 data-list2 e)
               nil)
        ;; ><><><
        ;; trunk can only return one data
        (d ('trunk (arrow-list data-list)))
        => (let ((data-list (mapcar (lambda (x) (bs/deep bs x))
                                    data-list)))
             (match (apply/arrow-list/filter arrow-list data-list e)
               () => nil
               (a) => (match (apply/arrow a (list data-list bs ns))
                        ((d1 . _) bs1 ns1) =>
                        (unify/one d d1 (list ds bs1 ns1)))
               (a1 a2 . _) => nil))
        (('trunk (arrow-list data-list)) d)
        => (let ((data-list (mapcar (lambda (x) (bs/deep bs x))
                                    data-list)))
             (match (apply/arrow-list/filter arrow-list data-list e)
               () => nil
               (a) => (match (apply/arrow a (list data-list bs ns))
                        ((d1 . _) bs1 ns1) =>
                        (unify/one d d1 (list ds bs1 ns1)))
               (a1 a2 . _) => nil))))))

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

(assert
 (equal

  (mapcar
   #'parse/top
   '((dt natural (-> type)
      zero (-> natural)
      succ (natural -> natural))

     (df add (natural natural -> natural)
      (:m zero -> :m)
      (:m :n succ -> :m :n recur succ))

     (df mul (natural natural -> natural)
      (:m zero -> zero)
      (:m :n succ -> :m :n recur :m add))

     (ap (->
          zero succ
          zero succ succ
          add))))

  '((dt ((natural (nil ((n type)))) ((zero (nil ((n natural)))) (succ (((n natural)) ((n natural)))))))
    (df ((add (((n natural) (n natural)) ((n natural)))) ((((v (:m 0)) (n zero)) ((v (:m 0)))) (((v (:m 0)) (v (:n 0)) (n succ)) ((v (:m 0)) (v (:n 0)) (n recur) (n succ))))))
    (df ((mul (((n natural) (n natural)) ((n natural)))) ((((v (:m 0)) (n zero)) ((n zero))) (((v (:m 0)) (v (:n 0)) (n succ)) ((v (:m 0)) (v (:n 0)) (n recur) (v (:m 0)) (n add))))))
    (ap (nil ((n zero) (n succ) (n zero) (n succ) (n succ) (n add)))))))


(assert
 (equal

  (mapcar
   #'parse/top
   '((dt vector ((:t :> type) number :t -> type)
      null (-> zero :t vector)
      cons (:n :t vector :t -> :n succ :t vector))

     (df map (:n :t1 vector (:t1 -> :t2) -> :n :t2 vector)
      (null :f -> null)
      (:l :e cons :f -> :e :f apply :l :f map cons))

     (df append (:m :t vector :n :t vector -> :m :n add :t vector)
      (null :l -> :l)
      (:l :e cons :l1 -> :l :l1 append :e cons))))

  '((dt ((vector (((b (((v (:t 1))) ((n type)) nil)) (n number) (v (:t 0))) ((n type)))) ((null (nil ((n zero) (v (:t 0)) (n vector)))) (cons (((v (:n 0)) (v (:t 0)) (n vector) (v (:t 0))) ((v (:n 0)) (n succ) (v (:t 0)) (n vector)))))))
    (df ((map (((v (:n 0)) (v (:t1 0)) (n vector) (a (((v (:t1 0))) ((v (:t2 0)))))) ((v (:n 0)) (v (:t2 0)) (n vector)))) ((((n null) (v (:f 0))) ((n null))) (((v (:l 0)) (v (:e 0)) (n cons) (v (:f 0))) ((v (:e 0)) (v (:f 0)) (n apply) (v (:l 0)) (v (:f 0)) (n map) (n cons))))))
    (df ((append (((v (:m 0)) (v (:t 0)) (n vector) (v (:n 0)) (v (:t 0)) (n vector)) ((v (:m 0)) (v (:n 0)) (n add) (v (:t 0)) (n vector)))) ((((n null) (v (:l 0))) ((v (:l 0)))) (((v (:l 0)) (v (:e 0)) (n cons) (v (:l1 0))) ((v (:l 0)) (v (:l1 0)) (n append) (v (:e 0)) (n cons)))))))))

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
          (:success e1) => e1
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
      (:fail report) => (list :fail
                              report))))

(defun check/arrow (type-formal-arrow a e)
  ;; type-formal-arrow, formal-arrow, env -> check-report
  (match (pass1/arrow type-formal-arrow ())
    (tac tsc) =>
    (match (apply/cedent tac e)
      (ds0 bs0 ns0) =>
      (match (pass1/arrow a ())
        (ac sc) =>
        (let ((e1 (unify
                   (type-apply/cedent
                    ac
                    (list (cons 'unify-point ds0)
                          bs0
                          ns0)))))
          (if (not e1)
              (list :fail
                    (list `(check/arrow
                            can not unify
                            (tac ,tac)
                            (ac ,ac)
                            (env ,(type-apply/cedent
                                   ac
                                   (list (cons 'unify-point ds0)
                                         bs0
                                         ns0))))))
              (let* ((e2 (type-apply/cedent sc e1)))
                (match e2
                  (ds2 bs2 ns2) =>
                  (if (unify
                       (apply/cedent
                        tsc
                        (list (cons 'unify-point ds2)
                              bs2
                              ns2)))
                      (list :success e)
                      (list :fail
                            (list `(check/arrow
                                    can not unify
                                    (tsc ,tsc)
                                    (sc ,sc)))))))))))))

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
