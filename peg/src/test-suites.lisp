(in-package #:peg)
#+5am
(5am:def-suite peg-suite
 :description "Suite containing all tests")

#+5am
(5am:def-suite peg-grammar-suite
 :in peg-suite
 :description "Suite for grammar tests")

