;;; This file includes functions for parsing comments
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* comment-suite :in grammar-suite)

; EndLine <- LF / CRLF / CR
(define-parent-expr end-line
  (or-expr 
    (literal-char-terminal #\cr) 
    (literal-char-terminal #\lf) 
    (compose
      (literal-char-terminal #\cr) 
      (literal-char-terminal #\lf))))
(5am:test line-end-test
  (5am:is 
    (funcall 'end-line
             (list #\CR)))
  (5am:is (eq NIL
    (funcall 'end-line
             (coerce "   jigaro" 'list)))))

(define-parent-expr comment-line 
  (compose 
    (zero-or-more 
      (literal-char-terminal #\space))
    (literal-char-terminal #\#) 
    (zero-or-more 
      (compose
        (negative-lookahead 'line-end)
        (char-terminal)))))
(5am:test comment-line-test
  (5am:is 
    (funcall 'comment-line
             (coerce "   ### jigaro" 'list)))
  (5am:is (eq NIL
    (funcall 'comment-line
             (coerce "jigaro" 'list)))))

; ComEndLine <- SP* ('# ' Comment)? EndLine
(define-parent-expr comment-endline
  (compose 
    (zero-or-more (literal-char-terminal #\SP)) 
    (optional-expr 'comment-line)
    'line-end))
(5am:test comment-endline-test
  (5am:is 
    (funcall 'comment-endline 
             (list #\# #\Newline)))
  (5am:is (eq NIL
    (funcall 'comment-endline
             (coerce "jigaro" 'list)))))

; Spacing <- ComEndLine? SP+
(define-parent-expr spacing
  (compose
    (optional-expr 'comment-endline)
    (one-or-more (literal-char-terminal #\SP))))
(5am:test comment-endline-test
  (5am:is 
    (funcall 'spacing
             (list #\SP #\SP)))
  (5am:is 
    (funcall 'spacing
             (list #\# #\Newline #\SP #\SP)))
  (5am:is (eq NIL
    (funcall 'spacing
             (list #\# #\SP #\SP)))))

