;;; This file includes functions for parsing literals
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* literal-suite :in grammar-suite)

(defpattern upper-case (char-range #\A #\Z))
#+5am
(5am:test upper-case-test
          (5am:is
           (funcall #'upper-case
             (coerce "First" 'list) 0))
          (5am:is (eq NIL
                      (funcall 'upper-case
                        (coerce "jigaro" 'list) 0))))

(defpattern lower-case (char-range #\a #\z))
#+5am
(5am:test lower-case-test
          (5am:is
           (funcall 'lower-case
             (coerce "first" 'list) 0))
          (5am:is (eq NIL
                      (funcall 'lower-case
                        (coerce "Jigaro" 'list) 0))))

; represents a single-quoted string, e.g. 'hello'
(defpattern string-literal
         (compose
          (char-literal #\')
          (one-or-more
           (compose
            (negative-lookahead (char-literal #\'))
            #'any-char))
          (char-literal #\')))
#+5am
(5am:test string-literal-test
          (5am:is
           (funcall
               'string-literal
             (coerce "'something'" 'list) 0))
          (5am:is (eq NIL
                      (funcall
                          'string-literal
                        (coerce "'33DaThing" 'list) 0))))
; parses a literal range of chars, e.g. 'a-z' or 
; '0-9'. Ranges must have a dash and must not start
; with a dash.
(defpattern char-range-literal
         (compose
          (negative-lookahead (char-literal #\-))
          #'any-char
          (char-literal #\-)
          (negative-lookahead (char-literal #\-))
          #'any-char))
#+5am
(5am:test char-range-literal-test
          (5am:is
           (funcall
               'char-range-literal
             (coerce "a-zforeva" 'list) 0))
          (5am:is (eq NIL
                      (funcall
                          'char-range-literal
                        (coerce "a--zforeva" 'list) 0)))
          (5am:is (eq NIL
                      (funcall
                          'char-range-literal
                        (coerce "-a--zforeva" 'list) 0))))

(defpattern digit (char-range #\0 #\9))
#+5am
(5am:test digit-test
          (5am:is
           (funcall 'digit
             (coerce "0bacon" 'list) 0))
          (5am:is (eq NIL
                      (funcall 'digit
                        (coerce "oneBacon" 'list) 0))))

; parses upper-case hex letters and digits
(defpattern uphex
         (or-expr
          (char-range #\A #\Z)
          (char-range #\0 #\9)))
#+5am
(5am:test uphex-test
          (5am:is (funcall 'uphex
                    (coerce "56CAFE" 'list) 0))
          (5am:is (funcall 'uphex
                    (coerce "CAFE" 'list) 0))
          (5am:is (eq NIL (funcall 'uphex
                            (coerce "hunky" 'list) 0))))

; Unicode <- 'u' ('10' uphex{4} / uphex{4,5})
(defpattern unicode
         (compose (char-literal #\u)
                  (or-expr
                   (compose
                    (string-expr "10")
                    (times 'uphex 4))
                   (or-expr (times 'uphex 5) (times 'uphex 4)))))
#+5am
(5am:test unicode-test
          (5am:is (funcall 'unicode
                    (coerce "u10AAFFforyou" 'list) 0))
          (5am:is (funcall 'unicode
                    (coerce "uFEFECEC" 'list) 0))
          (5am:is (eq NIL (funcall 'unicode
                            (coerce "CAFE" 'list) 0)))
          (5am:is (eq NIL (funcall 'unicode
                            (coerce "uCAF" 'list) 0))))

; Parses against a regex-style set of char options,
; including ranges, e.g. [A-Za-z0-9] 
(defpattern range-expr
         (compose
          (char-literal #\[)
          (one-or-more
           (compose
            (negative-lookahead (char-literal #\]))
            (or-expr
             #'char-range-literal
             #'any-char)))
          (char-literal #\])))
#+5am
(5am:test range-expr-test
          (5am:is
           (funcall
               'range-expr
             (coerce "[`A-Za-z0-9_]" 'list) 0))
          (5am:is (eq NIL
                      (funcall
                          'range-expr
                        (coerce "[]|`A-Za-z0-9_" 'list) 0))))

; represents literals for unicode, ranges of chars, or quoted
; strings 
(defpattern simple
         (or-expr
          'unicode
          'range-expr
          'string-literal))
#+5am
(5am:test simple-test
          (5am:is
           (funcall
               'simple
             (coerce "[`A-Za-z0-9_]" 'list) 0))
          (5am:is
           (funcall
               'simple
             (coerce "u10AAFFforyou" 'list) 0))
          (5am:is (eq NIL
                      (funcall
                          'simple
                        (coerce "[]|`A-Za-z0-9_" 'list) 0))))
