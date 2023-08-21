(in-package #:peg-grammar)

#+5am
(5am:in-suite grammar-suite)

(defun weird-sum (a b) (+ a b))

#+5am
(5am:test weird-sum-test
    (5am:is (eq 3 (weird-sum 1 2))))

