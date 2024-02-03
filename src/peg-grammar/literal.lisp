;;; This file includes functions for parsing literals
;;; in peg grammar.

(in-package #:cl-peg-yapp/peg-grammar)

(interpol:enable-interpol-syntax)

#+5am
(5am:def-suite* literal-suite :in grammar-suite)

(defpattern arrow
            (or-expr (string-expr "<-")
                     (char-literal #\LEFTWARDS_ARROW)))
#+5am
(5am:test arrow-test
          (5am:is
           (test-input #'arrow "<-"))
          (5am:is 
           (eq NIL
               (test-input #'arrow "--"))))

(defpattern upper-case (char-range #\A #\Z))
#+5am
(5am:test upper-case-test
          (5am:is
           (test-input #'upper-case "First"))
          (5am:is (eq NIL
                      (test-input #'upper-case "jigaro"))))

(defpattern lower-case (char-range #\a #\z))
#+5am
(5am:test lower-case-test
          (5am:is
           (test-input #'lower-case "first"))
          (5am:is (eq NIL
           (test-input #'lower-case "Jigaro"))))

(defexpr escaped-single-quote
        (escaped-char (char-literal #\')))
#+5am
(5am:test string-literal-test
  (5am:is
   (test-input #'escaped-single-quote "\\\'")))

(defexpr escaped-double-quote
        (escaped-char (char-literal #\")))

(defexpr escaped-hyphen
        (escaped-char (char-literal #\-)))

(defexpr escaped-square-bracket
        (escaped-char (char-literal #\])))

; represents a single-quoted string, e.g. 'hello'
(defexpr string-literal
         (or-expr 
           (compose
             (char-literal #\")
             (one-or-more
               (compose
                 (negative-lookahead 
                   (char-literal #\"))
                 (or-expr
                   #'escaped-double-quote
                   #'any-char)))
             (char-literal #\"))
           (compose
             (char-literal #\')
             (one-or-more
               (compose
                 (negative-lookahead 
                   (char-literal #\'))
                 (or-expr
                   #'escaped-single-quote
                   #'any-char)))
             (char-literal #\'))))
#+5am
(5am:test string-literal-test
  (5am:is
   (test-input #'string-literal "'\\\''"))
  (5am:is
   (test-input #'string-literal "\"some\\\"thing\""))
  (5am:is
   (test-input #'string-literal "\"\\\"\""))
  (5am:is
   (test-input #'string-literal "'somet\\'hing'"))
  (5am:is
   (test-input #'string-literal "'something'"))
  (5am:is (eq NIL
              (test-input #'string-literal "33Dathing'"))))

; parses a literal range of chars, e.g. 'a-z' or 
; '0-9'. Ranges must have a dash and must not start
; with a dash.
; Note that if a hyphen is escaped, it will appear
; as a child node either as the start range or end of
; the range.
(defexpr char-range-literal
         (compose
          (negative-lookahead (char-literal #\-))
          (or-expr 
            #'escaped-hyphen
            #'any-char)
          (char-literal #\-)
          (negative-lookahead (char-literal #\-))
          (or-expr 
            #'escaped-hyphen
            #'any-char)))
#+5am
(5am:test char-range-literal-test
  ; start escaped
  (5am:is
   (test-input #'char-range-literal "\\--a-zforeva"))
  ; end escaped
  (5am:is
   (test-input #'char-range-literal "a-\\-zforeva"))
  (5am:is
   (test-input #'char-range-literal "a-zforeva"))
  (5am:is 
   (eq NIL
       (test-input #'char-range-literal "a--zforeva")))
  (5am:is 
   (eq NIL
       (test-input #'char-range-literal "-a--zforeva"))))

(defpattern digit (char-range #\0 #\9))
#+5am
(5am:test digit-test
  (5am:is 
   (test-input #'digit "0bacon"))
  (5am:is (eq NIL
              (test-input #'digit "onebacon"))))

; specialized digit expr for parsing purposes
(defexpr exact-amount (one-or-more #'digit))
(defexpr min-amount (one-or-more #'digit))
(defexpr max-amount (one-or-more #'digit))

; parses upper-case hex letters and digits
(defpattern uphex
         (or-expr
          (char-range #\A #\Z)
          (char-range #\0 #\9)))
#+5am
(5am:test uphex-test
  (5am:is (test-input #'uphex "56CAFE"))
  (5am:is (test-input #'uphex "CAFE"))
  (5am:is (eq NIL (test-input #'uphex "hunAFE"))))

; Unicode <- 'u' ('10' uphex{4} / uphex{4,5})
(defexpr unicode
         (compose (char-literal #\u)
                  (or-expr
                   (compose
                    (string-expr "10")
                    (times 'uphex 4))
                   (or-expr (times 'uphex 5) (times 'uphex 4)))))
#+5am
(5am:test unicode-test
          (5am:is (test-input #'unicode "u10AAFFforyou"))
          ; unicode latin block start
          (5am:is (test-input #'unicode "u0000"))
          ; unicode latin block end
          (5am:is (test-input #'unicode "u007F"))
          (5am:is (test-input #'unicode "uFEFECEC"))
          (5am:is (eq NIL 
                      (test-input #'unicode "CAFE")))
          (5am:is (eq NIL 
                      (test-input #'unicode "uCAF"))))

(defexpr range-char-option
         (or-expr 
           #'escaped-square-bracket
           #'any-char))
#+5am
(5am:test range-char-option-test
  (5am:is
   (test-input #'range-char-option "\\]_"))
  (5am:is
   (test-input #'range-char-option "`_")))

; Parses against a regex-style set of char options,
; including ranges, e.g. [A-Za-z0-9] 
; Range <- '[' (!']' ((. '-' .) / .))+ ']'
(defexpr range-expr
         (compose
          (char-literal #\[)
          (one-or-more
           (compose
            (negative-lookahead (char-literal #\]))
            (or-expr 
              #'char-range-literal
              #'range-char-option)))
          (char-literal #\])))
#+5am
(5am:test range-expr-test
  (5am:is
   (range-expr
     (coerce #?"[\\]]" 'list) 0))
  (5am:is
   (range-expr
     (coerce "[`\\]_]" 'list) 0))
  (5am:is
   (range-expr
     (coerce "[A-Z\\--a]" 'list) 0))
  (5am:is
   (range-expr
     (coerce "[`_]" 'list) 0))
  (5am:is
   (range-expr
     (coerce "[`A-Za-z\\]0-9_]" 'list) 0))
  (5am:is
   (range-expr
     (coerce "[`A-Z\\--a-z\\]0-9_]" 'list) 0))
  (5am:is (eq NIL
              (funcall
                'range-expr
                (coerce "[]|`A-Za-z0-9_" 'list) 0))))

; represents literals for unicode, ranges of chars, or quoted
; strings 
(defexpr simple
         (or-expr
           'unicode
           'range-expr
           'string-literal
           ; we're also adding here character classes
           'alphanum-class
           'alpha-class
           'numeric-class
           'wildcard-class
           ))
#+5am
(5am:test 
 simple-test
 (5am:is
  (funcall
    'simple
    (coerce "[`A-Za-z0-9_]" 'list) 0))
 (5am:is
  (funcall
    'simple
    (coerce "u10AAFFforyou" 'list) 0))
 (5am:is
  (simple (coerce "alphanum" 'list) 0))
 (5am:is (eq NIL
             (funcall
               'simple
               (coerce "[]|`A-Za-z0-9_" 'list) 0))))

; Name            <- [A-Za-z][A-Za-z0-9_]*
(defexpr name
         (compose
           (or-expr #'upper-case #'lower-case)
           (zero-or-more 
             (or-expr #'upper-case 
                      #'lower-case
                      #'digit))))
#+5am
(5am:test name-test
  (5am:is (test-full-match #'name "URL"))
  (5am:is (test-full-match #'name "Url"))
  (5am:is (test-full-match #'name "UrlName")))

;; All of these definitions parse 
;; the literal that represents a particular character
;; class. It's up to the parser to actually do something
;; with the denoted class

(defexpr unipoint-class
         (string-expr "unipoint"))

(defexpr numeric-class
         (string-expr "numeric"))

(defexpr alpha-class
         (string-expr "alpha"))

(defexpr alphanum-class
         (string-expr "alphanum"))

(defexpr wildcard-class
         (string-expr "."))

