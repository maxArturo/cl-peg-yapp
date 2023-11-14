(in-package #:cl)

;; needed to enable interpol syntax
(interpol:enable-interpol-syntax)

(defpackage #:peg
  (:use #:cl #:trivia)
  (:export #:peg-suite
           #:parser-suite
           #:grammar-suite
           #:scanner-suite))

(defpackage #:peg-parser
  (:use #:cl #:peg)
  (:export
   #:terminal
   #:parent
   #:char-terminal
   #:char-literal
   #:char-range
   #:positive-lookahead
   #:negative-lookahead
   #:compose
   #:times
   #:zero-or-more
   #:one-or-more
   #:optional-expr
   #:or-expr
   #:string-expr
   #:define-parent-expr))

(defpackage #:peg-grammar
  (:use #:cl #:peg #:peg-parser))

(defpackage #:peg-scanner
  (:use #:cl #:peg #:peg-grammar))
