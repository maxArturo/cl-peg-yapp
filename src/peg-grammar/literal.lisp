;;; This file includes functions for parsing literals
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* literal-suite :in grammar-suite)

(peg-patterns:define-parent-expr upper-case (peg-patterns:char-range-terminal #\A #\Z))
(5am:test upper-case-test
  (5am:is 
    (funcall 'upper-case
             (coerce "First" 'list)))
  (5am:is (eq NIL
    (funcall 'upper-case
             (coerce "jigaro" 'list)))))

(peg-patterns:define-parent-expr lower-case (peg-patterns:char-range-terminal #\a #\z))
(5am:test lower-case-test
  (5am:is 
    (funcall 'lower-case
             (coerce "first" 'list)))
  (5am:is (eq NIL
    (funcall 'lower-case
             (coerce "Jigaro" 'list)))))

; represents a single-quoted string, e.g. 'hello'
(peg-patterns:define-parent-expr string-literal
  (peg-patterns:compose
    (peg-patterns:literal-char-terminal #\') 
    (peg-patterns:one-or-more
      (peg-patterns:compose
        (peg-patterns:negative-lookahead (peg-patterns:literal-char-terminal #\')) 
        (peg-patterns:char-terminal)))
    (peg-patterns:literal-char-terminal #\')))
(5am:test string-literal-test
  (5am:is 
    (funcall 
      'string-literal
      (coerce "'something'" 'list)))
  (5am:is (eq NIL
    (funcall 
      'string-literal
      (coerce "'33DaThing" 'list)))))
; parses a literal range of chars, e.g. 'a-z' or 
; '0-9'. Ranges must have a dash and must not start
; with a dash.
(peg-patterns:define-parent-expr char-range-literal
  (peg-patterns:compose 
    (peg-patterns:negative-lookahead (peg-patterns:literal-char-terminal #\-))
    (peg-patterns:char-terminal)
    (peg-patterns:literal-char-terminal #\-)
    (peg-patterns:negative-lookahead (peg-patterns:literal-char-terminal #\-))
    (peg-patterns:char-terminal)))
#+5am
(5am:test char-range-literal-test
  (5am:is 
    (funcall 
      'char-range-literal
      (coerce "a-zforeva" 'list)))
  (5am:is (eq NIL
    (funcall 
      'char-range-literal
      (coerce "a--zforeva" 'list))))
  (5am:is (eq NIL
    (funcall 
      'char-range-literal
      (coerce "-a--zforeva" 'list))))
  )

(peg-patterns:define-parent-expr digit (peg-patterns:char-range-terminal #\0 #\9))
(5am:test digit-test
  (5am:is 
    (funcall 'digit
             (coerce "0bacon" 'list)))
  (5am:is (eq NIL
    (funcall 'digit
             (coerce "oneBacon" 'list)))))

; parses upper-case hex letters and digits
(peg-patterns:define-parent-expr uphex
  (peg-patterns:or-expr 
    (peg-patterns:char-range-terminal #\A #\Z)
    (peg-patterns:char-range-terminal #\0 #\9)))
#+5am
(5am:test uphex-test
  (5am:is (funcall 'uphex
      (coerce "56CAFE" 'list)))
  (5am:is (funcall 'uphex
      (coerce "CAFE" 'list)))
    (5am:is (eq NIL (funcall 'uphex
      (coerce "hunky" 'list)))))

; Unicode <- 'u' ('10' uphex{4} / uphex{4,5})
(peg-patterns:define-parent-expr unicode
  (peg-patterns:compose (peg-patterns:literal-char-terminal #\u)
      (peg-patterns:or-expr
        (peg-patterns:compose
          (peg-patterns:string-expr "10")
          (peg-patterns:times 'uphex 4))
        (peg-patterns:or-expr (peg-patterns:times 'uphex 5) (peg-patterns:times 'uphex 4)))))
#+5am
(5am:test unicode-test
  (5am:is (funcall 'unicode
      (coerce "u10AAFFforyou" 'list)))
  (5am:is (funcall 'unicode
      (coerce "uFEFECEC" 'list)))
  (5am:is (eq NIL(funcall 'unicode
    (coerce "CAFE" 'list))))
  (5am:is (eq NIL(funcall 'unicode
    (coerce "uCAF" 'list)))))

; Parses against a regex-style set of char options,
; including ranges, e.g. [A-Za-z0-9] 
(peg-patterns:define-parent-expr range-expr
  (peg-patterns:compose 
    (peg-patterns:literal-char-terminal #\[)
    (peg-patterns:one-or-more
      (peg-patterns:compose
        (peg-patterns:negative-lookahead (peg-patterns:literal-char-terminal #\]))
        (peg-patterns:or-expr 
          'char-range-literal
          (peg-patterns:char-terminal))))
    (peg-patterns:literal-char-terminal #\])))
#+5am
(5am:test range-expr-test
  (5am:is 
    (funcall 
      'range-expr
      (coerce "[`A-Za-z0-9_]" 'list)))
  (5am:is (eq NIL
    (funcall 
      'range-expr
      (coerce "[]|`A-Za-z0-9_" 'list)))))

; represents literals for unicode, ranges of chars, or quoted
; strings 
(peg-patterns:define-parent-expr simple
  (peg-patterns:or-expr
    'unicode
    'range-expr
    'string-literal))
#+5am
(5am:test simple-test
  (5am:is 
    (funcall 
      'simple 
      (coerce "[`A-Za-z0-9_]" 'list)))
  (5am:is 
    (funcall 
      'simple 
      (coerce "u10AAFFforyou" 'list)))
  (5am:is (eq NIL
    (funcall 
      'simple
      (coerce "[]|`A-Za-z0-9_" 'list)))))

