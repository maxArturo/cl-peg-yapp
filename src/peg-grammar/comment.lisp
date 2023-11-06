;;; This file includes functions for parsing comments
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* comment-suite :in grammar-suite)

(defun line-end ()
  "parses all combinations of a line ending"
  (parent-expr 
    (or-expr 
        (literal-char-terminal #\cr) 
        (literal-char-terminal #\lf) 
        (compose
          (literal-char-terminal #\cr) 
          (literal-char-terminal #\lf)))
    :line-end))
(5am:test line-end-test
  (5am:is 
    (funcall (line-end)
             (list #\CR)))
  (5am:is (eq NIL
    (funcall (line-end)
             (coerce "   jigaro" 'list)))))

(defun comment-line ()
  "parses a full comment line"
  (parent-expr 
    (compose 
      (zero-or-more 
        (literal-char-terminal #\space))
      (literal-char-terminal #\#) 
      (zero-or-more 
        (compose
          (negative-lookahead (line-end))
          (char-terminal))))
    :comment-line))
(5am:test comment-line-test
  (5am:is 
    (funcall (comment-line) 
             (coerce "   ### jigaro" 'list)))
  (5am:is (eq NIL
    (funcall (comment-line) 
             (coerce "jigaro" 'list)))))

(defun comment-endline ()
  (parent-expr 
    (compose (comment-line) (line-end))
    :comment-endline))
(5am:test comment-endline-test
  (5am:is 
    (funcall (comment-endline) 
             (list #\# #\Newline)))
  (5am:is (eq NIL
    (funcall (comment-endline) 
             (coerce "jigaro" 'list)))))

; Spacing <- ComEndLine? SP+
(defun spacing ()
  (parent-expr (compose
    (optional-expr (comment-endline))
    (one-or-more (literal-char-terminal #\SP)))
    :spacing))
(5am:test comment-endline-test
  (5am:is 
    (funcall (spacing)
             (list #\# #\Newline #\SP #\SP)))
  (5am:is (eq NIL
    (funcall (spacing)
             (list #\# #\SP #\SP)))))

