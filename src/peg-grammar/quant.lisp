;;; This file includes functions for parsing quantifiers
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* quant-suite :in grammar-suite)

(defun min-max-amount ()
  "parses an amount modifier"
  (parent-expr
    (compose
      (literal-char-terminal #\{)
      (one-or-more (digit))
      (literal-char-terminal #\,)
      (one-or-more (digit))
      (literal-char-terminal #\}))
    :min-max-amount))
#+5am
(5am:test min-max-amount-test
  (5am:is (funcall (min-max-amount)
      (coerce "{83,85}" 'list)))
  (5am:is (eq NIL (funcall (uphex)
    (coerce "{83,999" 'list)))))

