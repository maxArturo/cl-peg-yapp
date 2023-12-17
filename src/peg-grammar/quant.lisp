;;; This file includes functions for parsing quantifiers
;;; in peg grammar.

(in-package #:cl-peg-yapp/peg-grammar)

#+5am
(5am:def-suite* quant-suite :in grammar-suite)

; MinMax     <-- '{' Min ',' Max? '}'
(defexpr min-max-amount
         (compose
          (char-literal #\{)
          (one-or-more #'min-amount)
          (char-literal #\,)
          (one-or-more #'max-amount)
          (char-literal #\})))
#+5am
(5am:test min-max-amount-test
          (5am:is (min-max-amount
                    (coerce "{83,85}" 'list) 0))
          (5am:is (eq NIL (funcall #'min-max-amount
                            (coerce "{83,999" 'list) 0))))

;Amount      <- '{' Count '}'
(defexpr amount
         (compose
          (char-literal #\{)
          (one-or-more 'digit)
          (char-literal #\})))
#+5am
(5am:test amount-test
          (5am:is (funcall #'amount
                    (coerce "{8}" 'list) 0))
          (5am:is (eq NIL (funcall #'amount
                            (coerce "{83,999}" 'list) 0))))

; Optional   <-- '?'
(defexpr optional (char-literal #\?))
#+5am
(5am:test optional-test
          (5am:is (funcall #'optional
                    (coerce "?butwhy" 'list) 0))
          (5am:is (eq NIL (funcall #'optional
                            (coerce "!buthwy" 'list) 0))))

; MinZero    <-- '*'
(defexpr min-zero (char-literal #\*))
#+5am
(5am:test min-zero-test
          (5am:is (funcall #'min-zero
                    (coerce "*butwhy" 'list) 0))
          (5am:is (eq NIL (funcall #'min-zero
                            (coerce "!buthwy" 'list) 0))))

; MinOne     <-- '+'
(defexpr min-one (char-literal #\+))
#+5am
(5am:test min-one-test
          (5am:is (funcall #'min-one
                    (coerce "+butwhy" 'list) 0))
          (5am:is (eq NIL (funcall #'min-one
                            (coerce "!buthwy" 'list) 0))))

(defexpr quant
         (or-expr
          #'optional
          #'min-zero
          #'min-one
          #'min-max-amount
          #'amount))
#+5am
(5am:test quant-test
          (5am:is (funcall #'quant
                    (coerce "{333} for you" 'list) 0))
          (5am:is (funcall #'quant
                    (coerce "?{3,33} for you" 'list) 0))
          (5am:is (funcall #'quant
                    (coerce "{3,33} for you" 'list) 0))
          (5am:is (eq NIL (funcall #'quant
                            (coerce "!buthwy" 'list) 0))))
