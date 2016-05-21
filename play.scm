;; use chez scheme

(print-graph #t)
(include "helper.scm")
(include "sequent1.scm")
(cover-check+)
(recur-check-)
