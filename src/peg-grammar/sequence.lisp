;;; This file includes functions for parsing sequences
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* sequence-suite :in grammar-suite)

; Expression <- Sequence (Spacing '/' SP+ Sequence)*
(defun expression ()
  (parent-expr (compose
    (sequence-expr) 
    (zero-or-more (compose 
      (spacing) 
      (literal-char-terminal #\/)
      (one-or-more (literal-char-terminal #\SP))
      (sequence-expr))))
    :expression))

; Sequence <- Rule (Spacing Rule)*
(defun sequence-expr ()
  (parent-expr 
    (compose
      (rule) 
      (zero-or-more (compose
        (spacing) (rule))))
    :sequence))

; Rule <- PosLook / NegLook / Plain
(defun rule ()
  (parent-expr (or-expr
    (pos-look) (neg-look) (plain))
    :rule))

; CheckId <- (upper lower+)+
(defun check-id ()
  (parent-expr (compose 
    (upper-case) (one-or-more (lower-case)))
    :check-id))
#+5am
(5am:test check-id-test
  (5am:is (funcall (check-id)
      (coerce "Helloworld" 'list)))
  (5am:is (eq NIL 
    (funcall (check-id)
      (coerce "HelloWorld" 'list)))))


; Plain <- Primary Quant?
(defun plain ()
  (parent-expr 
    (compose (primary) (optional-expr (quant)))
    :plain))

; PosLook <- '&' Primary Quant?
(defun pos-look ()
  (parent-expr 
    (compose (literal-char-terminal #\&) (plain))
    :pos-look))

; NegLook <- '!' Primary Quant?
(defun neg-look ()
  (parent-expr 
    (compose (literal-char-terminal #\!) (plain))
    :neg-look))

; Primary <- Simple / CheckId / '(' Expression ')'
(defun primary ()
  (parent-expr 
    (or-expr
      (simple) (check-id) 
      (compose 
        (literal-char-terminal #\()
        (expression)
        (literal-char-terminal #\))))
    :primary))

; ScanDef <- CheckId SP+ '<-'  SP+ Expression 
(defun scan-def ()
  (parent-expr 
    (compose
      (check-id)
      (one-or-more (literal-char-terminal #\SP))
      (string-expr "<-")
      (one-or-more (literal-char-terminal #\SP))
      (expression))
    :scan-def))


