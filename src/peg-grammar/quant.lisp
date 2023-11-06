;;; This file includes functions for parsing quantifiers
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* quant-suite :in grammar-suite)

(defun min-max-amount ()
  "parses an amount range modifier"
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

(defun amount ()
  "parses an amount modifier"
  (parent-expr
    (compose
      (literal-char-terminal #\{)
      (one-or-more (digit))
      (literal-char-terminal #\}))
    :amount))
#+5am
(5am:test amount-test
  (5am:is (funcall (amount)
      (coerce "{8}" 'list)))
  (5am:is (eq NIL (funcall (uphex)
    (coerce "{83,999}" 'list)))))


(defun optional ()
  "parses an optional modifier"
  (parent-expr
    (literal-char-terminal #\?)
    :optional))
#+5am
(5am:test optional-test
  (5am:is (funcall (optional)
      (coerce "?butwhy" 'list)))
  (5am:is (eq NIL (funcall (optional)
    (coerce "!buthwy" 'list)))))

(defun min-zero ()
  "parses a min-zero modifier"
  (parent-expr
    (literal-char-terminal #\*)
    :min-zero))
#+5am
(5am:test min-zero-test
  (5am:is (funcall (min-zero)
      (coerce "*butwhy" 'list)))
  (5am:is (eq NIL (funcall (min-zero)
    (coerce "!buthwy" 'list)))))

(defun min-one ()
  "parses a min-one modifier"
  (parent-expr
    (literal-char-terminal #\+)
    :min-one))
#+5am
(5am:test min-one-test
  (5am:is (funcall (min-one)
      (coerce "+butwhy" 'list)))
  (5am:is (eq NIL (funcall (min-one)
    (coerce "!buthwy" 'list)))))

(defun quant ()
  "parent expression group that captures all 
   PEG optional expressions"
  (parent-expr (or-expr
    (optional)                 
    (min-zero)
    (min-one)
    (min-max-amount)
    (amount))
    :quant
    ))
#+5am
(5am:test quant-test
  (5am:is (funcall (quant)
      (coerce "{333} for you" 'list)))
  (5am:is (funcall (quant)
      (coerce "?{3,33} for you" 'list)))
  (5am:is (funcall (quant)
      (coerce "{3,33} for you" 'list)))
  (5am:is (eq NIL (funcall (min-one)
    (coerce "!buthwy" 'list)))))

