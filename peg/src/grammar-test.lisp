(in-package #:peg-tests)

#+5am
(5am:in-suite peg-grammar-suite)

#+5am
(5am:test weird-sum-test
    (5am:is (eq 3 (peg-grammar:weird-sum 1 2))))

