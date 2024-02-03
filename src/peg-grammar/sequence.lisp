;;; This file includes functions for parsing sequences
;;; in peg grammar.

(in-package #:cl-peg-yapp/peg-grammar)
(interpol:enable-interpol-syntax)

#+5am
(5am:def-suite* sequence-suite :in grammar-suite)

; Seq             <- Prefix*
(defexpr sequence-expr
         (one-or-more #'prefix))
#+5am
(5am:test sequence-expr-test
  (test-full-match #'sequence-expr "'+'" )
  (test-full-match #'sequence-expr "!']' &'['")
  (test-full-match #'sequence-expr "!']' ('a'/ 'b')")
  (test-full-match #'sequence-expr "(!numeric/ 'b')"))

; Prefix          <- PosLook / NegLook / Suffix
(defpattern prefix
         (or-expr 
           #'pos-look
           #'neg-look
           #'suffix))

;PosLook    <- '&' Suffix
(defexpr pos-look
         (compose (char-literal #\&) #'suffix))

; NegLook    <- '!' Suffix
(defexpr neg-look
         (compose (char-literal #\!) #'suffix))
(5am:test 
 neg-look-test
 (5am:is (neg-look
           (coerce "!']'" 'list) 0)))


; Suffix          <- Primary Spacing (Quant Spacing)?
(defexpr suffix
         (compose
           #'primary
           #'spacing
           (opt-expr (compose #'quant #'spacing))))

; Primary         <- '(' Exp ')'
;                  / Simple
;                  / '.'
;                  / Name Spacing !Arrow
(defexpr primary
         (or-expr
           (compose
             (char-literal #\()
             #'expression
             (char-literal #\)))
           #'simple
           (char-literal #\.)
           (compose
             #'name
             #'spacing
             (negative-lookahead #'arrow))))
#+5am
(5am:test primary-test
  (test-full-match #'primary ".")
  (test-full-match #'primary "URL")
  (test-input
    (compose #'name #'spacing) "URL")
  (test-full-match #'primary "'333'")
  (test-full-match #'primary "333" :other-value nil))

; Definition      <- Name Spacing Arrow Exp
(defexpr definition
         (compose
           #'name
           #'spacing
           #'arrow
           #'expression))

(interpol:enable-interpol-syntax)
#+5am
(5am:test scan-def-test
  (test-full-match #'definition "AposChunk <- '\\\''")
  (test-full-match #'definition "AddExpr  <- ('+'/'-') Factor")
  (test-full-match #'definition "First <- [a-d]")
  (test-full-match #'definition "HelloWorld" :other-value nil) 
  (test-full-match #'definition "" :other-value nil) 
  (test-full-match #'definition #?"\
    Simple <- '[' (!']' (alphanum '-' alphanum / alphanum) )+ ']'
    / numeric"))

; Alternative     <- Seq ('/' Spacing Seq)*
(defexpr alternative 
         (compose
           #'sequence-expr
           (zero-or-more 
             (compose
               (char-literal #\/)
               #'spacing
               #'sequence-expr))))
