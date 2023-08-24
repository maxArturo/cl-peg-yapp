;; base parser
;; heavily based on the original paper and
;; https://github.com/PhilippeSigaud/Pegged/wiki/PEG-Basics

(in-package #:peg-parser)

(defstruct expression
  kind
  value)

