;; use chez scheme

(print-graph #t)
(include "define-macro.scm")
(include "flower-barcket.scm")
(include "match.scm")
(include "helper.scm")
(include "sequent1.scm")
(cover-check+)
(recur-check+)
