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
          (let ((new (vector
                      ;; for testing
                      ;; (gensym)
                      symbol)))
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
    (((var (#(:m) nil)) (name zero))
     ((var (#(:m) nil))))
    (((var (#(:m) nil)) (var (#(:n) nil)) (name succ))
     ((var (#(:m) nil)) (var (#(:n) nil)) (name recur) (name succ)))
    (((bind (((var (#(:t) nil))) ((name type)) nil)) (var (#(:t) nil)))
     ((name type)))
    (((bind (((var (#(:t) nil))) ((name type)) :true)) (var (#(:t) nil)))
     ((name type)))
    (((bind (((var (#(:t) 2))) ((name type)) nil)) (var (#(:t) nil)))
     ((name type)))
    (((bind (((var (#(:t1) nil)) (var (#(:t2) 2)) (var (#(:t3) 0))) ((name j) (name k)) nil)) (var (#(:t) nil)))
     ((name type)))
    (((bind (((var (#(:t) 2))) ((name type)) :true)) (var (#(:t) nil)))
     ((name type)))
    (((var (#(:t) nil))
      (arrow (((var (#(:t) nil))) ((var (#(:t) nil))))))
     ((arrow (((var (#(:t) nil))) ((arrow (((var (#(:t) nil))) ((var (#(:t) nil))))) (var (#(:t) nil))))) (name type))))))

(defun apply/dispatch (d e)
  ;; data, env -> env
  (match (list d e)
    (('name name) (ds bs ns))
    => (let* ((s (><><><)))
         (match s
           ('function fa fas) => ()
           ('type-cons fa arity name-list) => ()
           ('data-cons fa arity name) => ()))
    (('var id level) (ds bs ns))
    => ()
    (('arrow ac sc) (ds bs ns))
    => (apply/arrow (list ac sc) e)
    (('bind var cedent live?) (ds bs ns))
    => ()))

(defun apply/arrow (a e)
  ;; arrow, env -> env
  (match (list a e)
    ((ac sc) (ds bs ns))) =>
    (let* ((e1 (unify (run/cedent ac (list (cons 'unify-point ds) bs ns)))))
      (if e1
          (apply/cedent sc e1)
          ;; ><><><
          ;; need gc after succedent
          #f)))

(defun apply/cedent (c e)
  ;; cedent, env -> env
  (match c
    () => e
    (h . r) => (apply/cedent r (apply/dispatch h e))))

(defun unify (e)
  ;; env -> env
  ;; 'unify-point
  )

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
