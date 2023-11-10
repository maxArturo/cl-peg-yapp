;;; This file includes functions for parsing sequences
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* sequence-suite :in grammar-suite)

; Expression <- Sequence (Spacing '/' SP+ Sequence)*
(peg-parser:define-parent-expr expression
  (peg-parser:compose
    'sequence-expr
    (peg-parser:zero-or-more (peg-parser:compose 
      'spacing  
      (peg-parser:literal-char-terminal #\/)
      (peg-parser:one-or-more (peg-parser:literal-char-terminal #\SP))
      'sequence-expr))))

; Sequence <- Rule (Spacing Rule)*
(peg-parser:define-parent-expr sequence-expr
  (peg-parser:compose
    'rule
    (peg-parser:zero-or-more (peg-parser:compose
      'spacing 'rule))))

; Rule <- PosLook / NegLook / Plain
(peg-parser:define-parent-expr rule
  (peg-parser:or-expr 'pos-look 'neg-look 'plain))

; CheckId <- (upper lower+)+
(peg-parser:define-parent-expr check-id
  (peg-parser:compose 'upper-case (peg-parser:one-or-more 'lower-case)))
#+5am
(5am:test check-id-test
  (5am:is (funcall 'check-id
      (coerce "Helloworld" 'list)))
  (5am:is (eq NIL 
    (funcall 'check-id
      (coerce "helloWorld" 'list)))))

; Plain <- Primary Quant?
(peg-parser:define-parent-expr plain
  (peg-parser:compose 'primary (peg-parser:optional-expr 'quant)))
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
(peg-parser:define-parent-expr pos-look
  (peg-parser:compose (peg-parser:literal-char-terminal #\&) 'plain))

; NegLook <- '!' Primary Quant?
(peg-parser:define-parent-expr neg-look
  (peg-parser:compose (peg-parser:literal-char-terminal #\!) 'plain))

; Primary <- Simple / CheckId / '(' Expression ')'
(peg-parser:define-parent-expr primary
  (peg-parser:or-expr
     'simple 'check-id
      (peg-parser:compose 
        (peg-parser:literal-char-terminal #\()
        'expression
        (peg-parser:literal-char-terminal #\)))))

; ScanDef <- CheckId SP+ '<-'  SP+ Expression 
(peg-parser:define-parent-expr definition
  (peg-parser:compose
    'check-id
    (peg-parser:one-or-more (peg-parser:literal-char-terminal #\SP))
    (peg-parser:or-expr (peg-parser:string-expr "<-") 
      (peg-parser:literal-char-terminal #\LEFTWARDS_ARROW))
    (peg-parser:one-or-more (peg-parser:literal-char-terminal #\SP))
   'expression))
#+5am
(5am:test scan-def-test
  (5am:is (funcall 'definition
      (coerce "First <- [a-d]" 'list)))
  (5am:is (eq NIL 
    (funcall 'definition
      (coerce "HelloWorld" 'list)))))

