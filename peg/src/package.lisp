(in-package :cl)

(defpackage #:peg
  (:use #:cl #:trivia)
  (:export #:peg-suite #:peg-grammar-suite))

(defpackage #:peg-grammar
  (:use #:cl #:peg)
  (:export #:weird-sum))

(defpackage #:peg-tests
  (:use #:cl #:trivia #:peg))
 
