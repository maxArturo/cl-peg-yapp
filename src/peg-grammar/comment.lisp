;;; This file includes functions for parsing comments
;;; in peg grammar.

(in-package #:cl-peg-yapp/peg-grammar)

#+5am
(5am:def-suite* comment-suite :in grammar-suite)

; EndLine <- LF / CRLF / CR
(defexpr end-line
         (or-expr
          (compose
           (char-literal #\cr)
           (char-literal #\lf))
          (char-literal #\cr)
          (char-literal #\lf)))
#+5am
(5am:test end-line-test
          (5am:is
           (funcall #'end-line
             (list #\CR #\LF) 0))
          (5am:is (eq NIL
                      (funcall #'end-line
                        (coerce "   jigaro" 'list) 0))))

(defexpr comment-line
         (compose
          (zero-or-more
           (char-literal #\space))
          (char-literal #\#)
          (zero-or-more
           (compose
            (negative-lookahead #'end-line)
            #'any-char))))
#+5am
(5am:test comment-line-test
          (5am:is
           (funcall #'comment-line
             (coerce "   ### jigaro" 'list) 0))
          (5am:is (eq NIL
                      (funcall #'comment-line
                        (coerce "jigaro" 'list) 0))))

; ComEndLine <- SP* ('# ' Comment)? EndLine
(defexpr comment-endline
         (compose
           (opt-expr #'comment-line)
           #'end-line))
#+5am
(5am:test comment-endline-test
          (5am:is
           (funcall 'comment-endline
             (list #\# #\Newline) 0))
          (5am:is (eq NIL
                      (funcall 'comment-endline
                        (coerce "jigaro" 'list) 0))))

; Spacing <- ComEndLine? SP+
(defexpr spacing
         (compose
           (opt-expr #'comment-endline)
           (one-or-more (char-literal #\SP))))
#+5am
(5am:test spacing-test
  (5am:is
   (funcall #'spacing
            (list #\SP #\SP) 0))
  (5am:is
   (funcall #'spacing
            (list #\# #\Newline #\SP #\SP) 0))
  (5am:is (eq NIL
              (funcall #'spacing
                       (list #\# #\SP #\SP) 0))))
