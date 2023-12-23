;;; This file includes functions for parsing sequences
;;; in peg grammar.

(in-package #:cl-peg-yapp/peg-grammar)
(interpol:enable-interpol-syntax)

#+5am
(5am:def-suite* sequence-suite :in grammar-suite)

; Expression <- Sequence (Spacing* '/' SP* Sequence)* Spacing*
(defexpr expression
         (compose
           #'sequence-expr
           (zero-or-more 
             (compose
               (zero-or-more #'spacing)
               (char-literal #\/)
               (zero-or-more #'spacing)
               #'sequence-expr))))
#+5am
(5am:test expression-test
  (test-full-match #'expression "'+'/'-'")
  (test-full-match #'expression "((alphanum '-' alphanum) / alphanum)")
  (test-full-match #'expression "alphanum / numeric")
  (test-full-match #'expression #?"\
Modifier* (ClassDeclaration
			     / EnumDeclaration
			     / InterfaceDeclaration
			     / AnnotationTypeDeclaration) / SEMI"))

; Sequence <- Rule (Spacing Rule)*
(defexpr sequence-expr
         (compose
           #'rule
           (zero-or-more 
             (compose
               #'spacing #'rule))))
#+5am
(5am:test sequence-expr-test
  (test-full-match #'sequence-expr "'+'" )
  (test-full-match #'sequence-expr "!']' &'['")
  (test-full-match #'sequence-expr "!']' ('a'/ 'b')")
  (test-full-match #'sequence-expr "(!numeric/ 'b')"))

; CheckId <- (upper lower+)+
(defexpr check-id
         (or-expr
           (one-or-more 
             (compose #'upper-case (one-or-more #'lower-case)))
           (one-or-more #'upper-case)))
#+5am
(5am:test check-id-test
  (test-full-match #'check-id "AddExpr")
  (test-full-match #'check-id "Helloworld")
  (test-full-match #'check-id "elloWorld" :other-value nil))

; Rule <- PosLook / NegLook / Plain
(defexpr rule
         (or-expr #'pos-look #'neg-look #'plain))

; Plain <- Primary Quant?
(defexpr plain (compose #'primary (opt-expr #'quant)))
#+5am
(5am:test plain-test
  (test-full-match #'plain "Helloworld")
  (test-full-match #'plain "AddExpr*")
  (test-full-match #'plain "helloWorld" :other-value nil) 
  (test-full-match #'plain "333" :other-value nil))

; PosLook <- '&' Primary Quant?
(defexpr pos-look
         (compose (char-literal #\&) #'primary
                  (opt-expr #'quant)))

; NegLook <- '!' Primary Quant?
(defexpr neg-look
         (compose (char-literal #\!) #'primary
                  (opt-expr #'quant)))
(5am:test 
 neg-look-test
 (5am:is (neg-look
           (coerce "!']'" 'list) 0)))

; Primary <- Simple / CheckId / '(' Expression ')'
(defexpr primary
         (or-expr
           #'simple #'check-id
           (compose
             (char-literal #\()
             (zero-or-more (char-literal #\SP))
             #'expression
             (zero-or-more (char-literal #\SP))
             (char-literal #\)))))
#+5am
(5am:test primary-test
  (test-full-match #'primary "('+'/'-')")
  (test-full-match #'primary "(!']' (alphanum '-' alphanum / alphanum) )")
  (test-full-match #'primary "333" :other-value nil))

; ScanDef <- CheckId SP+ '<-'  SP+ Expression 
(defexpr definition
         (compose
           #'check-id
           (zero-or-more #'end-line)
           (one-or-more (or-expr (char-literal #\SP) (char-literal #\TAB)))
           (or-expr (string-expr "<-")
                    (char-literal #\LEFTWARDS_ARROW))
           (one-or-more (char-literal #\SP))
           #'expression))

(interpol:enable-interpol-syntax)
#+5am
(5am:test scan-def-test
  (test-full-match #'definition "AddExpr  <- ('+'/'-') Factor")
  (test-full-match #'definition "First <- [a-d]")
  (test-full-match #'definition "HelloWorld" :other-value nil) 
  (test-full-match #'definition "" :other-value nil) 
  (test-full-match #'definition #?"\
    Simple <- '[' (!']' (alphanum '-' alphanum / alphanum) )+ ']'
    / numeric"))

