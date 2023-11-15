;;; This file includes functions for parsing comments
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* comment-suite :in grammar-suite)

; EndLine <- LF / CRLF / CR
(peg-parser:define-parent-expr end-line
  (peg-parser:or-expr 
    (peg-parser:char-literal #\cr) 
    (peg-parser:char-literal #\lf) 
    (peg-parser:compose
      (peg-parser:char-literal #\cr) 
      (peg-parser:char-literal #\lf))))
#+5am
(5am:test end-line-test
  (5am:is 
    (funcall #'end-line
             (list #\CR) 0))
  (5am:is (eq NIL
    (funcall #'end-line
             (coerce "   jigaro" 'list) 0))))

(peg-parser:define-parent-expr comment-line 
  (#'peg-parser:compose 
    (#'peg-parser:zero-or-more 
      (peg-parser:char-literal #\space))
    (peg-parser:char-literal #\#) 
    (peg-parser:zero-or-more 
      (peg-parser:compose
        (peg-parser:negative-lookahead 'end-line)
        #'peg-parser:any-char))))
#+5am
(5am:test comment-line-test
  (5am:is 
    (funcall #'comment-line
             (coerce "   ### jigaro" 'list) 0))
  (5am:is (eq NIL
    (funcall #'comment-line
             (coerce "jigaro" 'list) 0))))

; ComEndLine <- SP* ('# ' Comment)? EndLine
(peg-parser:define-parent-expr comment-endline
  (peg-parser:compose 
    (peg-parser:zero-or-more (peg-parser:char-literal #\SP)) 
    (peg-parser:optional #'comment-line)
    #'end-line))
#+5am
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
    (peg-parser:optional 'comment-endline)
    (peg-parser:one-or-more (peg-parser:char-literal #\SP))))
#+5am
(5am:test comment-endline-test
  (5am:is 
    (funcall #'spacing
             (list #\SP #\SP) 0))
  (5am:is 
    (funcall 'spacing
             (list #\# #\Newline #\SP #\SP)))
  (5am:is (eq NIL
    (funcall 'spacing
             (list #\# #\SP #\SP)))))

