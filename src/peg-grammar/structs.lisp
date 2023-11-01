;;;; structs used for peg tree representation

(in-package #:peg-grammar)

(defstruct terminal
  value)

(defstruct parent
  kind
  children)

(defparameter *empty-terminal* (make-terminal :value :empty-success))

