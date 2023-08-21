(in-package #:cl-peg)

;; full test suite definition here
;; most tests will live next to their contextual unit functions

#+5am
(5am:def-suite peg-tests :description "peg test suite")


(in-package #:peg-grammar)
#+5am
(5am:def-suite grammar-suite
  :description "Test grammar functionality"
  :in peg-tests)

