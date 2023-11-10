;;; This file includes functions for parsing comments
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* comment-suite :in grammar-suite)

; EndLine <- LF / CRLF / CR
(peg-patterns:define-parent-expr end-line
  (peg-patterns:or-expr 
    (peg-patterns:literal-char-terminal #\cr) 
    (peg-patterns:literal-char-terminal #\lf) 
    (peg-patterns:compose
      (peg-patterns:literal-char-terminal #\cr) 
      (peg-patterns:literal-char-terminal #\lf))))
(5am:test end-line-test
  (5am:is 
    (funcall 'end-line
             (list #\CR)))
  (5am:is (eq NIL
    (funcall 'end-line
             (coerce "   jigaro" 'list)))))

(peg-patterns:define-parent-expr comment-line 
  (peg-patterns:compose 
    (peg-patterns:zero-or-more 
      (peg-patterns:literal-char-terminal #\space))
    (peg-patterns:literal-char-terminal #\#) 
    (peg-patterns:zero-or-more 
      (peg-patterns:compose
        (peg-patterns:negative-lookahead 'end-line)
        (peg-patterns:char-terminal)))))
(5am:test comment-line-test
  (5am:is 
    (funcall 'comment-line
             (coerce "   ### jigaro" 'list)))
  (5am:is (eq NIL
    (funcall 'comment-line
             (coerce "jigaro" 'list)))))

; ComEndLine <- SP* ('# ' Comment)? EndLine
(peg-patterns:define-parent-expr comment-endline
  (peg-patterns:compose 
    (peg-patterns:zero-or-more (peg-patterns:literal-char-terminal #\SP)) 
    (peg-patterns:optional-expr 'comment-line)
    'end-line))
(5am:test comment-endline-test
  (5am:is 
    (funcall 'comment-endline 
             (list #\# #\Newline)))
  (5am:is (eq NIL
    (funcall 'comment-endline
             (coerce "jigaro" 'list)))))

; Spacing <- ComEndLine? SP+
(peg-patterns:define-parent-expr spacing
  (peg-patterns:compose
    (peg-patterns:optional-expr 'comment-endline)
    (peg-patterns:one-or-more (peg-patterns:literal-char-terminal #\SP))))
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

