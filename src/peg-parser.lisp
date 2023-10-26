;;;; parser for actual peg grammar file

;;; Here we include PEG non terminals which will be
;;; considered parent nodes.

(in-package #:peg-parser)

#+5am
(5am:in-suite grammar-suite)

(defun line-end (input)
  "parses all combinations of a line ending"
  (funcall (or-expr 
    (literal-char-terminal #\cr) 
    (literal-char-terminal #\lf) 
    (compose
      (literal-char-terminal #\cr) 
      (literal-char-terminal #\lf)))
    input))

(defun comment-line (input)
  "parses a full comment line"
  (funcall (compose 
    (zero-or-more 
      (literal-char-terminal #\space))
    (literal-char-terminal #\#) 
    (zero-or-more 
      (compose
        (negate #'line-end)
        #'char-terminal))
    (optional #'line-end)) input))


