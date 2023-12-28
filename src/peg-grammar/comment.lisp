;;; This file includes functions for parsing comments
;;; in peg grammar.

(in-package #:cl-peg-yapp/peg-grammar)

#+5am
(5am:def-suite* comment-suite :in grammar-suite)

; EndLine <- LF / CRLF / CR
(defpattern end-line
         (or-expr
          (compose
           (char-literal #\cr)
           (char-literal #\lf))
          (char-literal #\cr)
          (char-literal #\lf)))
#+5am
(5am:test end-line-test
          (5am:is
           (end-line
             (list #\CR #\LF) 0))
          (5am:is 
           (eq NIL
               (test-input #'end-line "   jigaro"))))

; Space <- ' ' / u0009 / Eol
(defpattern space-expr 
            (or-expr 
              (char-literal #\space)
              (char-literal #\tab)
              #'end-line))

; Comment         <- ('#' / '//') (!Eol .)* Eol
(defpattern comment
         (compose
          (or-expr 
            (char-literal #\#)
            (string-expr "//"))
          (zero-or-more
           (compose
            (negative-lookahead #'end-line)
            #'any-char))
          #'end-line))
#+5am
(5am:test comment-test
          (5am:is
           (test-input #'comment (format nil "### jigaro~&~&")))
          (5am:is (eq NIL
                      (test-input #'comment "jigaro"))))

; Spacing  <- (Space / Comment)*   # Spaces And Comments
(defpattern spacing
            (zero-or-more
             (or-expr 
               #'space-expr
               #'comment)))

