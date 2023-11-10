;;; This file includes functions for parsing sequences
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* sequence-suite :in grammar-suite)

; Expression <- Sequence (Spacing '/' SP+ Sequence)*
(peg-patterns:define-parent-expr expression
  (peg-patterns:compose
    'sequence-expr
    (peg-patterns:zero-or-more (peg-patterns:compose 
      'spacing  
      (peg-patterns:literal-char-terminal #\/)
      (peg-patterns:one-or-more (peg-patterns:literal-char-terminal #\SP))
      'sequence-expr))))

; Sequence <- Rule (Spacing Rule)*
(peg-patterns:define-parent-expr sequence-expr
  (peg-patterns:compose
    'rule
    (peg-patterns:zero-or-more (peg-patterns:compose
      'spacing 'rule))))

; Rule <- PosLook / NegLook / Plain
(peg-patterns:define-parent-expr rule
  (peg-patterns:or-expr 'pos-look 'neg-look 'plain))

; CheckId <- (upper lower+)+
(peg-patterns:define-parent-expr check-id
  (peg-patterns:compose 'upper-case (peg-patterns:one-or-more 'lower-case)))
#+5am
(5am:test check-id-test
  (5am:is (funcall 'check-id
      (coerce "Helloworld" 'list)))
  (5am:is (eq NIL 
    (funcall 'check-id
      (coerce "helloWorld" 'list)))))

; Plain <- Primary Quant?
(peg-patterns:define-parent-expr plain
  (peg-patterns:compose 'primary (peg-patterns:optional-expr 'quant)))
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
(peg-patterns:define-parent-expr pos-look
  (peg-patterns:compose (peg-patterns:literal-char-terminal #\&) 'plain))

; NegLook <- '!' Primary Quant?
(peg-patterns:define-parent-expr neg-look
  (peg-patterns:compose (peg-patterns:literal-char-terminal #\!) 'plain))

; Primary <- Simple / CheckId / '(' Expression ')'
(peg-patterns:define-parent-expr primary
  (peg-patterns:or-expr
     'simple 'check-id
      (peg-patterns:compose 
        (peg-patterns:literal-char-terminal #\()
        'expression
        (peg-patterns:literal-char-terminal #\)))))

; ScanDef <- CheckId SP+ '<-'  SP+ Expression 
(peg-patterns:define-parent-expr definition
  (peg-patterns:compose
    'check-id
    (peg-patterns:one-or-more (peg-patterns:literal-char-terminal #\SP))
    (peg-patterns:or-expr (peg-patterns:string-expr "<-") 
      (peg-patterns:literal-char-terminal #\LEFTWARDS_ARROW))
    (peg-patterns:one-or-more (peg-patterns:literal-char-terminal #\SP))
   'expression))
#+5am
(5am:test scan-def-test
  (5am:is (funcall 'definition
      (coerce "First <- [a-d]" 'list)))
  (5am:is (eq NIL 
    (funcall 'definition
      (coerce "HelloWorld" 'list)))))

