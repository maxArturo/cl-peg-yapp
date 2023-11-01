(in-package #:cl)

;; needed to enable interpol syntax
(interpol:enable-interpol-syntax)

(defpackage #:peg
  (:use #:cl #:trivia)
  (:export #:peg-suite 
           #:grammar-suite
           #:parser-suite))

(defpackage #:peg-grammar
  (:use #:cl #:peg))

(defpackage #:peg-parser
  (:use #:cl #:peg))

