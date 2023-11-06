;;; This file includes functions for parsing literals
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* literal-suite :in grammar-suite)

(defun string-literal ()
  "represents a single-quoted string, e.g. 'hello'"
  (parent-expr
    (compose
      (literal-char-terminal #\') 
      (one-or-more
        (compose
          (negative-lookahead (literal-char-terminal #\')) 
          (char-terminal)))
      (literal-char-terminal #\'))
    :string-literal))
(5am:test string-literal-test
  (5am:is 
    (funcall 
      (string-literal)
      (coerce "'something'" 'list)))
  (5am:is (eq NIL
    (funcall 
      (string-literal)
      (coerce "'33DaThing" 'list)))))

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

; Unicode <- 'u' ('10' uphex{4} / uphex{4,5})
(defun unicode ()
  "parses unicode literals."
  (parent-expr
    (compose (literal-char-terminal #\u)
      (or-expr
        (compose
          (string-expr "10")
          (times (uphex) 4))
        (or-expr (times (uphex) 5) (times (uphex) 4))))
    :unicode))
#+5am
(5am:test unicode-test
  (5am:is (funcall (unicode)
      (coerce "u10AAFFforyou" 'list)))
  (5am:is (funcall (unicode)
      (coerce "uFEFECEC" 'list)))
  (5am:is (eq NIL(funcall (unicode)
    (coerce "CAFE" 'list))))
  (5am:is (eq NIL(funcall (unicode)
    (coerce "uCAF" 'list)))))

(defun range-expr ()
  "Parses against a regex-style set of char options,
   including ranges, e.g. [A-Za-z0-9]"
  (parent-expr
    (compose 
      (literal-char-terminal #\[)
      (one-or-more
        (compose
          (negative-lookahead (literal-char-terminal #\]))
          (or-expr 
            (char-range-literal)
            (char-terminal))))
      (literal-char-terminal #\]))
    :range-expr))
#+5am
(5am:test range-expr-test
  (5am:is 
    (funcall 
      (range-expr)
      (coerce "[`A-Za-z0-9_]" 'list)))
  (5am:is (eq NIL
    (funcall 
      (range-expr)
      (coerce "[]|`A-Za-z0-9_" 'list)))))

(defun simple ()
  "represents literals for unicode, ranges of chars, or quoted
   strings"
  (parent-expr
    (or-expr
      (unicode)
      (range-expr)
      (string-literal))
    :simple))
#+5am
(5am:test simple-test
  (5am:is 
    (funcall 
      (simple)
      (coerce "[`A-Za-z0-9_]" 'list)))
  (5am:is 
    (funcall 
      (simple)
      (coerce "u10AAFFforyou" 'list)))
  (5am:is (eq NIL
    (funcall 
      (simple)
      (coerce "[]|`A-Za-z0-9_" 'list)))))

