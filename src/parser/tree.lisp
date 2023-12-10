;;; This file includes tree representation code for a compacted
;;; version of the match trees.

(in-package #:cl-peg-yapp/peg-parser)

#+5am
(5am:def-suite* tree-suite :in parser-suite)

(defparameter *compacted-tree* t)

(defparameter *print-match-error* nil)

(defvar *tree-hash*)

(defvar *packrat-enabled* nil)

(defun parse (parse-expr grammar)
  (declare (string grammar))
  (let ((*packrat-enabled* t)
        (*tree-hash* (make-hash-table))
        (grammar-list (coerce grammar 'list)))
    (funcall parse-expr grammar-list 0)))

