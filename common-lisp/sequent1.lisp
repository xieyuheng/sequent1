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
  (list (parse/cedent (left-of '-> s))
          (parse/cedent (right-of '-> s))))

(defun parse/cedent (s)
  ;; sexp-cedent -> formal-cedent
  (match s
    () => ()
    (h . r) => (cons (parse/dispatch h)
                     (parse/cedent r))))

(defun parse/dispatch (v)
  ;; sexp-form -> formal-form
  (flet ((var? (v) (keywordp v))
         (name? (v) (and (symbolp v) (not (keywordp v))))
         (arrow? (v) (and (listp v) (member '-> v)))
         (im-bind? (v) (and (listp v) (member :> v)))
         (ex-bind? (v) (and (listp v) (member '@ v))))
    (cond ((var? v) (list 'v (parse/var v)))
          ((name? v) (list 'n v))
          ((arrow? v) (list 'a (parse/arrow v)))
          ((im-bind? v) (list 'b
                              (list (parse/cedent (left-of :> v))
                                    (parse/cedent (right-of :> v))
                                    nil)))
          ((ex-bind? v) (list 'b
                              (list (parse/cedent (left-of '@ v))
                                    (parse/cedent (right-of '@ v))
                                    :true))))))

(defun parse/var (v)
  ;; keyword -> formal-var
  (let* ((string (symbol-name v))
         (cursor (find-char "^" string)))
    (if cursor
        (list (intern (subseq string 0 cursor) :keyword)
              (parse-integer string
                             :start (+ 1 cursor)
                             :junk-allowed t
                             :radix 10))
        (list v nil))))

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

  '((((n natural) (n natural))
     ((n natural)))
    (((n natural) (n natural))
     ((a (((n natural) (n natural)) ((n natural)))) (n natural)))
    (((v (:m nil)) (n zero))
     ((v (:m nil))))
    (((v (:m nil)) (v (:n nil)) (n succ))
     ((v (:m nil)) (v (:n nil)) (n recur) (n succ)))
    (((b (((v (:t nil))) ((n type)) nil)) (v (:t nil)))
     ((n type)))
    (((b (((v (:t nil))) ((n type)) :true)) (v (:t nil)))
     ((n type)))
    (((b (((v (:t 2))) ((n type)) nil)) (v (:t nil)))
     ((n type)))
    (((b (((v (:t1 nil)) (v (:t2 2)) (v (:t3 0))) ((n j) (n k)) nil))
      (v (:t nil))) ((n type)))
    (((b (((v (:t 2))) ((n type)) :true)) (v (:t nil)))
     ((n type))))))

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

  '((((name natural) (name natural))
     ((name natural)))
    (((name natural) (name natural))
     ((arrow (((name natural) (name natural)) ((name natural)))) (name natural)))
    (((var (#(:m nil) nil)) (name zero))
     ((var (#(:m nil) nil))))
    (((var (#(:m nil) nil)) (var (#(:n nil) nil)) (name succ))
     ((var (#(:m nil) nil)) (var (#(:n nil) nil)) (name recur) (name succ)))
    (((bind (((var (#(:t nil) nil))) ((name type)) nil)) (var (#(:t nil) nil)))
     ((name type)))
    (((bind (((var (#(:t nil) nil))) ((name type)) :true)) (var (#(:t nil) nil)))
     ((name type)))
    (((bind (((var (#(:t nil) 2))) ((name type)) nil)) (var (#(:t nil) nil)))
     ((name type)))
    (((bind (((var (#(:t1 nil) nil)) (var (#(:t2 nil) 2)) (var (#(:t3 nil) 0))) ((name j) (name k)) nil)) (var (#(:t nil) nil)))
     ((name type)))
    (((bind (((var (#(:t nil) 2))) ((name type)) :true)) (var (#(:t nil) nil)))
     ((name type)))
    (((var (#(:t nil) nil)) (arrow (((var (#(:t nil) nil))) ((var (#(:t nil) nil))))))
     ((arrow (((var (#(:t nil) nil))) ((arrow (((var (#(:t nil) nil))) ((var (#(:t nil) nil))))) (var (#(:t nil) nil))))) (name type))))))

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
  (match (list a e)
    ((ac sc) (ds bs ns)) =>
    (let* ((e1 (unify
                (run/cedent
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
    ('bind b) => (apply/arrow b e)))

(defun apply/literal-arrow (a e)
  (match e
    (ds bs ns) =>
    (list (cons (list 'arrow a)
                ds)
          bs
          ns)))

(defun bs/find (bs v)
  ;; bs, var -> data or nil
  (match v
    (id level) =>
    (let ((found/commit (assoc level (id->ls id) :test #'eq)))
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
      ('trunk (name ds))
      => (list 'trunk
               (list name
                     (mapcar (lambda (x) (bs/deep bs x))
                             ds))))))

(defun apply/var (v e)
  ;; var, env -> env
  (match e
    (ds bs ns) =>
    (list (cons (bs/deep bs v) ds)
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
            => (apply/name/function n f e)
            ('type-constructor (type/arrow arity data-name-list))
            => (apply/arity n arity e)
            ('data-constructor (type/arrow arity type-name))
            => (apply/arity n arity e))))))

(defun data/non-determinate? (d e)
  ;; data, env -> bool
  (match (bs/walk (env->bs e) d)
    ('var v) => t
    ('arrow _) => nil
    ('cons _) => nil
    ('trunk (name data-list))
    => (data-list/non-determinate? data-list e)))

(defun data-list/non-determinate? (data-list e)
  ;; data list, env -> bool
  (find-if (lambda (x) (data/non-determinate? x e))
           data-list))

(defun apply/name/function (n f e)
  ;; name, function, env -> env
  (match (list f e)
    ((type/arrow arity body) (ds bs ns)) =>
    (labels ((recur (body e)
               (if (eq body ())
                   (orz ()
                     ("apply/name/function function : ~a" f))
                   (let ((result (apply/arrow (car body) e)))
                     (if result
                         result
                         (recur (cdr body) e))))))
      (let ((args (subseq ds 0 arity)))
        (if (data-list/non-determinate? args e)
            (list (cons (list 'trunk
                              (list n args))
                        (subseq ds arity))
                  bs
                  ns)
            (recur body e))))))

(defun apply/arity (n arity e)
  ;; name, arity, env -> env
  (match e
    (ds bs ns) =>
    (list (cons (list 'cons
                      (list n (subseq ds 0 arity)))
                (subseq ds arity))
          bs
          ns)))

(defun bs/extend (bs v d)
  ;; bs var data -> bs
  (match v
    (id level) =>
    (let* ((level (if (eq nil level)
                      1
                      level))
           (found/ls (assoc id bs :test #'eq)))
      (if found/ls
          (substitute (cons id (cons (cons level d)
                                     (cdr found/ls)))
                      (lambda (pair) (eq (car pair) id))
                      bs)
          (cons (cons id (list (cons level d)))
                bs)))))

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
                                  (bs/extend bs v d1)
                                  ns)))))
        (recur vs e)))))

(defun unify (e)
  ;; env -> env of nil
  (match e
    (ds bs ns) =>
    (let* ((l1 (left-of 'unify-point ds))
           (tmp (right-of 'unify-point ds))
           (l2 (subseq tmp 0 (length l1)))
           (ds1 (subseq tmp (* 2 (length l1)))))
      (unify/list l1 l2 (list ds1 bs ns)))))

(defun unify/list (l1 l2 e)
  ;; data list, data list, env => env or nil
  (cond ((eq nil e) nil)
        ((eq () l1) e)
        (:else
         (unify/list (cdr l1) (cdr l2)
                     (unify/one (car l1) (car l2) e)))))

(defun unify/one (d1 d2 e)
  ;; data, data, env => env or nil
  (match e
    (ds bs ns) =>
    (let ((d1 (bs/walk bs d1))
          (d2 (bs/walk bs d2)))
      (match (list d1 d2)
        (('var v) d) => (list ds (bs/extend bs v d) ns)
        (d ('var v)) => (list ds (bs/extend bs v d) ns)
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
        (d ('trunk (name data-list)))
        => (if (data-list/non-determinate? data-list e)
               nil
               (apply/name
                name
                (list (append (mapcar (lambda (x) (bs/deep bs x))
                                      data-list)
                              ds)
                      bs
                      ns)))
        (('trunk (name data-list)) d)
        => (if (data-list/non-determinate? data-list e)
               nil
               (apply/name
                name
                (list (append (mapcar (lambda (x) (bs/deep bs x))
                                      data-list)
                              ds)
                      bs
                      ns)))))))

(defun eva (fs e)
  ;; formal-top list, env -> env
  (match fs
    () => e
    (h . r) => (eva r (eva/formal-top h e))))

(defun eva/formal-top (f e)
  ;; formal-top, env -> env
  (match (list f e)
    (('dt (fn fa) fnfas) (ds bs ns)) => ><><><
    (('df (fn fa) fas) (ds bs ns)) => ><><><
    (('ap fa) (ds bs ns)) => (apply/arrow (pass1/arrow fa ()) e)))
