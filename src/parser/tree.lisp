;;; This file includes tree representation code for a compacted
;;; version of the match trees.

(in-package #:peg-parser)

#+5am
(5am:def-suite* tree-suite :in parser-suite)

(defparameter *compacted-tree* nil)
#+nil
(defparameter *compacted-tree* t)

