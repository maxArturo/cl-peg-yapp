;;; This file includes tree representation code for a compacted
;;; version of the match trees.

(in-package #:peg-parser)

#+5am
(5am:def-suite* base-suite :in parser-suite)

; the current representation of the tree
(defvar *compact-peg-tree*)

