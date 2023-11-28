;;; This file includes tree representation code for a compacted
;;; version of the match trees.

(in-package #:peg-parser)

#+5am
(5am:def-suite* tree-suite :in parser-suite)

(defparameter *compacted-tree* t)

(defparameter *print-match-error* nil)

(defvar *tree-hash*)

(defvar *packrat-enabled* nil)

(defun parse (parse-expr grammar)
  (declare (string grammar))
  (let ((*packrat-enabled* t)
        (*tree-hash* (make-hash-table)))
    (funcall parse-expr (coerce grammar 'list) 0)))

#+nil
(parse 
  (or-expr
       (char-literal #\f)
       (char-literal #\i)
       (char-literal #\g)
       (char-literal #\a))
  "arigar"
  )

