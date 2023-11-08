;;; This file includes functions for parsing sequences
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* sequence-suite :in grammar-suite)

; Expression <- Sequence (Spacing '/' SP+ Sequence)*
(define-parent-expr expression
  (compose
    'sequence-expr
    (zero-or-more (compose 
      'spacing  
      (literal-char-terminal #\/)
      (one-or-more (literal-char-terminal #\SP))
      'sequence-expr))))

; Sequence <- Rule (Spacing Rule)*
(define-parent-expr sequence-expr
  (compose
    'rule
    (zero-or-more (compose
      'spacing 'rule))))

; Rule <- PosLook / NegLook / Plain
(define-parent-expr rule
  (or-expr 'pos-look 'neg-look 'plain))

; CheckId <- (upper lower+)+
(define-parent-expr check-id
  (compose 'upper-case (one-or-more 'lower-case)))
#+5am
(5am:test check-id-test
  (5am:is (funcall 'check-id
      (coerce "Helloworld" 'list)))
  (5am:is (eq NIL 
    (funcall 'check-id
      (coerce "helloWorld" 'list)))))

; Plain <- Primary Quant?
(define-parent-expr plain
  (compose 'primary (optional-expr 'quant)))
#+5am
(5am:test plain-test
  (5am:is (funcall 'plain
      (coerce "Helloworld" 'list)))
  (5am:is (eq NIL 
    (funcall 'plain
      (coerce "helloWorld" 'list))))
  (5am:is (eq NIL 
    (funcall 'plain
      (coerce "333" 'list)))))

; PosLook <- '&' Primary Quant?
(define-parent-expr pos-look
  (compose (literal-char-terminal #\&) 'plain))

; NegLook <- '!' Primary Quant?
(define-parent-expr neg-look
  (compose (literal-char-terminal #\!) 'plain))

; Primary <- Simple / CheckId / '(' Expression ')'
(define-parent-expr primary
  (or-expr
     'simple 'check-id
      (compose 
        (literal-char-terminal #\()
        'expression
        (literal-char-terminal #\)))))

; ScanDef <- CheckId SP+ '<-'  SP+ Expression 
(define-parent-expr definition
  (compose
    'check-id
    (one-or-more (literal-char-terminal #\SP))
    (or-expr (string-expr "<-") 
      (literal-char-terminal #\LEFTWARDS_ARROW))
    (one-or-more (literal-char-terminal #\SP))
   'expression))
#+5am
(5am:test scan-def-test
  (5am:is (funcall 'definition
      (coerce "First <- [a-d]" 'list)))
  (5am:is (eq NIL 
    (funcall 'definition
      (coerce "HelloWorld" 'list)))))

