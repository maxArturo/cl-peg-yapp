;;; This file includes functions for parsing literals
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* literal-suite :in grammar-suite)

(defun uphex ()
  "parses upper-case hex letters and digits."
  (parent-expr
    (or-expr 
      (char-range-terminal #\A #\Z)
      (char-range-terminal #\0 #\9))
    :uphex))
#+5am
(5am:test uphex-test
  (5am:is (funcall (uphex)
      (coerce "56CAFE" 'list)))
  (5am:is (funcall (uphex)
      (coerce "CAFE" 'list)))
    (5am:is (eq NIL (funcall (uphex)
      (coerce "hunky" 'list)))))

