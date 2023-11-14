;;;; structs used for peg tree representation

;;; Note that here, `start` is inclusive and `end` is exclusive.
;;; i.e. start is the index where the match begins, and end 
;;; is the index where subsequent matches would begin. A zero-length 
;;; match would have start == end. 

(in-package #:peg-parser)

(defstruct (match 
  (:constructor new-match (str start end &optional children kind)))
  (str nil :type list)
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  children
  (kind))

(defun empty-match (input i)
  (declare (list input) (fixnum i))
  (new-match input i i))

