;;;; classes used for peg tree representation

;;; Note that here, `start` is inclusive and `end` is exclusive.
;;; i.e. start is the index where the match begins, and end 
;;; is the index where subsequent matches would begin. A zero-length 
;;; match would have start == end. 

(in-package #:peg-parser)

(defstruct match
  (str nil :type list)
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  children
  kind)

(defun empty-match (input i)
  (declare (list input) (fixnum i))
  (new-match input i i))

(defun new-match (input start end &optional children kind)
  "instantiator for matches"
  (declare (list input) (fixnum start)
           (fixnum end))
  (compact-match 
    (make-match :str input :start start :end end
                :children children
                :kind kind)))

(defun compact-match (match-expr)
  "creates a compacted match with trimmed out
   terminal or nil children matches."
  (and 
    match-expr
    (if *compacted-tree* 
        (let* ((compacted (copy-match match-expr))
               (compacted-res 
                 (for:for 
                   ((child over (match-children compacted))
                    (valid-child = (and (match-kind child) child))
                    (valid-children = (remove-if (lambda (el) (not (match-kind el))) (match-children child)))
                    (candidate = (or valid-child valid-children))
                    (candidates when candidate collecting candidate)))))
          ; TODO wrap up pulling in child matches with non-nil kinds
          (print "ayoo")
          (print compacted-res)
          (setf (match-children compacted) 
                compated-res) compacted)
        match-expr)))
#+5am
(5am:test compact-match-test
  (5am:is
   (equalp (new-match nil 0 0)
           (compact-match 
             (new-match 
               nil 0 0 
               (list (new-match nil 0 0) 
                     (new-match nil 0 0) 
                     (new-match nil 0 0) 
                     (new-match nil 0 0))))))
  (5am:is
   (equalp (new-match nil 0 0 
                      (list (new-match nil 0 0 nil :testy)))
           (compact-match (new-match nil 0 0 
                                     (list (new-match nil 0 0) 
                                           (new-match nil 0 0) 
                                           (new-match nil 0 0 nil :testy)  
                                           (new-match nil 0 0) 
                                           (new-match nil 0 0)))))))

