;;;; parser for actual peg grammar file

;;; Here we include PEG non terminals which will be
;;; considered parent nodes.

(in-package #:peg-parser)

#+5am
(5am:in-suite grammar-suite)

(defun line-end ()
  "parses all combinations of a line ending"
  (lambda (input)
    (destructuring-bind 
      (&key result remainder)
      (funcall (or-expr 
        (literal-char-terminal #\cr) 
        (literal-char-terminal #\lf) 
        (compose
          (literal-char-terminal #\cr) 
          (literal-char-terminal #\lf)))
        input)
      (and 
        result 
        (list :result 
              (make-parent 
        :kind :line-end 
        :children result)
              :remainder
              remainder)))))

(defun comment-line ()
  "parses a full comment line"
  (lambda (input)
    (destructuring-bind 
      (&key result remainder)
      (funcall (compose 
        (zero-or-more 
          (literal-char-terminal #\space))
        (literal-char-terminal #\#) 
        (zero-or-more 
          (compose
            (negate (line-end))
            (char-terminal)))
        (optional (line-end))) input) 
      (and 
        result 
        (list :result 
              (make-parent 
        :kind :comment-line
        :children result)
              :remainder
              remainder)))))


