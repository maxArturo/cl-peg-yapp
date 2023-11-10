;;; This file includes functions for parsing comments
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* comment-suite :in grammar-suite)

; EndLine <- LF / CRLF / CR
(peg-parser:define-parent-expr end-line
  (peg-parser:or-expr 
    (peg-parser:literal-char-terminal #\cr) 
    (peg-parser:literal-char-terminal #\lf) 
    (peg-parser:compose
      (peg-parser:literal-char-terminal #\cr) 
      (peg-parser:literal-char-terminal #\lf))))
(5am:test end-line-test
  (5am:is 
    (funcall 'end-line
             (list #\CR)))
  (5am:is (eq NIL
    (funcall 'end-line
             (coerce "   jigaro" 'list)))))

(peg-parser:define-parent-expr comment-line 
  (peg-parser:compose 
    (peg-parser:zero-or-more 
      (peg-parser:literal-char-terminal #\space))
    (peg-parser:literal-char-terminal #\#) 
    (peg-parser:zero-or-more 
      (peg-parser:compose
        (peg-parser:negative-lookahead 'end-line)
        (peg-parser:char-terminal)))))
(5am:test comment-line-test
  (5am:is 
    (funcall 'comment-line
             (coerce "   ### jigaro" 'list)))
  (5am:is (eq NIL
    (funcall 'comment-line
             (coerce "jigaro" 'list)))))

; ComEndLine <- SP* ('# ' Comment)? EndLine
(peg-parser:define-parent-expr comment-endline
  (peg-parser:compose 
    (peg-parser:zero-or-more (peg-parser:literal-char-terminal #\SP)) 
    (peg-parser:optional-expr 'comment-line)
    'end-line))
(5am:test comment-endline-test
  (5am:is 
    (funcall 'comment-endline 
             (list #\# #\Newline)))
  (5am:is (eq NIL
    (funcall 'comment-endline
             (coerce "jigaro" 'list)))))

; Spacing <- ComEndLine? SP+
(peg-parser:define-parent-expr spacing
  (peg-parser:compose
    (peg-parser:optional-expr 'comment-endline)
    (peg-parser:one-or-more (peg-parser:literal-char-terminal #\SP))))
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

