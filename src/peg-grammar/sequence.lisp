;;; This file includes functions for parsing sequences
;;; in peg grammar.

(in-package #:peg-grammar)

#+5am
(5am:def-suite* sequence-suite :in grammar-suite)

; Expression <- Sequence (Spacing '/' SP+ Sequence)*
(def-exp expression
         (compose
          #'sequence-expr
          (zero-or-more (compose
                         #'spacing
                         (char-literal #\/)
                         (one-or-more (char-literal #\SP))
                         #'sequence-expr))))

; Sequence <- Rule (Spacing Rule)*
(def-exp sequence-expr
         (compose
          #'rule
          (zero-or-more (compose
                         #'spacing #'rule))))

; Rule <- PosLook / NegLook / Plain
(def-exp rule
         (or-expr #'pos-look #'neg-look #'plain))

; CheckId <- (upper lower+)+
(def-exp check-id
         (compose #'upper-case (one-or-more #'lower-case)))
#+5am
(5am:test check-id-test
          (5am:is (funcall #'check-id
                    (coerce "Helloworld" 'list) 0))
          (5am:is (eq NIL
                      (funcall #'check-id
                        (coerce "helloWorld" 'list) 0))))

; Plain <- Primary Quant?
(def-exp plain (compose #'primary (opt-expr #'quant)))
#+5am
(5am:test plain-test
          (5am:is (funcall #'plain
                    (coerce "Helloworld" 'list) 0))
          (5am:is (eq NIL
                      (funcall #'plain
                        (coerce "helloWorld" 'list) 0)))
          (5am:is (eq NIL
                      (funcall #'plain
                        (coerce "333" 'list) 0))))

; PosLook <- '&' Primary Quant?
(def-exp pos-look
         (compose (char-literal #\&) #'plain))

; NegLook <- '!' Primary Quant?
(def-exp neg-look
         (compose (char-literal #\!) #'plain))

; Primary <- Simple / CheckId / '(' Expression ')'
(def-exp primary
         (or-expr
          #'simple #'check-id
          (compose
           (char-literal #\()
           #'expression
           (char-literal #\)))))

; ScanDef <- CheckId SP+ '<-'  SP+ Expression 
(def-exp definition
         (compose
          #'check-id
          (one-or-more (char-literal #\SP))
          (or-expr (string-expr "<-")
                   (char-literal #\LEFTWARDS_ARROW))
          (one-or-more (char-literal #\SP))
          #'expression))
#+5am
(5am:test scan-def-test
          (5am:is (funcall #'definition
                    (coerce "First <- [a-d]" 'list) 0))
          (5am:is (eq NIL
                      (funcall #'definition
                        (coerce "HelloWorld" 'list) 0))))