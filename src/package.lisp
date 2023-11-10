(in-package #:cl)

;; needed to enable interpol syntax
(interpol:enable-interpol-syntax)

(defpackage #:peg
  (:use #:cl #:trivia)
  (:export #:peg-suite 
           #:patterns-suite
           #:grammar-suite
           #:parser-suite))

(defpackage #:peg-patterns
  (:use #:cl #:peg)
  (:export 
    #:terminal
    #:parent
    #:char-terminal
    #:literal-char-terminal
    #:char-range-terminal
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
  (:use #:cl #:peg #:peg-patterns))

(defpackage #:peg-parser
  (:use #:cl #:peg #:peg-grammar))

