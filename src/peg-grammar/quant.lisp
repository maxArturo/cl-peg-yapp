;;; This file includes functions for parsing quantifiers
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* quant-suite :in grammar-suite)

; MinMax     <-- '{' Min ',' Max? '}'
(peg-parser:define-parent-expr min-max-amount
  (peg-parser:compose
      (peg-parser:char-literal #\{)
      (peg-parser:one-or-more 'digit)
      (peg-parser:char-literal #\,)
      (peg-parser:one-or-more 'digit)
      (peg-parser:char-literal #\})))
#+5am
(5am:test min-max-amount-test
  (5am:is (funcall 'min-max-amount
      (coerce "{83,85}" 'list)))
  (5am:is (eq NIL (funcall 'min-max-amount
    (coerce "{83,999" 'list)))))

;Amount      <- '{' Count '}'
(peg-parser:define-parent-expr amount
  (peg-parser:compose
    (peg-parser:char-literal #\{)
    (peg-parser:one-or-more 'digit)
    (peg-parser:char-literal #\})))
#+5am
(5am:test amount-test
  (5am:is (funcall 'amount
      (coerce "{8}" 'list)))
  (5am:is (eq NIL (funcall 'amount
    (coerce "{83,999}" 'list)))))

; Optional   <-- '?'
(peg-parser:define-parent-expr optional (peg-parser:char-literal #\?))
#+5am
(5am:test optional-test
  (5am:is (funcall 'optional
      (coerce "?butwhy" 'list)))
  (5am:is (eq NIL (funcall 'optional
    (coerce "!buthwy" 'list)))))

; MinZero    <-- '*'
(peg-parser:define-parent-expr min-zero (peg-parser:char-literal #\*))
#+5am
(5am:test min-zero-test
  (5am:is (funcall 'min-zero
      (coerce "*butwhy" 'list)))
  (5am:is (eq NIL (funcall 'min-zero
    (coerce "!buthwy" 'list)))))

; MinOne     <-- '+'
(peg-parser:define-parent-expr min-one (peg-parser:char-literal #\+))
#+5am
(5am:test min-one-test
  (5am:is (funcall 'min-one 
      (coerce "+butwhy" 'list)))
  (5am:is (eq NIL (funcall 'min-one
    (coerce "!buthwy" 'list)))))

(peg-parser:define-parent-expr quant
  (peg-parser:or-expr
    'optional  
    'min-zero  
    'min-one   
    'min-max-amount
    'amount))
#+5am
(5am:test quant-test
  (5am:is (funcall 'quant
      (coerce "{333} for you" 'list)))
  (5am:is (funcall 'quant
      (coerce "?{3,33} for you" 'list)))
  (5am:is (funcall 'quant
      (coerce "{3,33} for you" 'list)))
  (5am:is (eq NIL (funcall 'quant
    (coerce "!buthwy" 'list)))))

