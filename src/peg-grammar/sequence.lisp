;;; This file includes functions for parsing sequences
;;; in peg grammar.

(in-package #:cl-peg-yapp/peg-grammar)

#+5am
(5am:def-suite* sequence-suite :in grammar-suite)

; Expression <- Sequence (Spacing* '/' SP* Sequence)*
(defexpr expression
  (compose
    #'sequence-expr
    (zero-or-more 
      (compose
        (zero-or-more #'spacing)
        (char-literal #\/)
        (zero-or-more (char-literal #\SP))
        #'sequence-expr))))
#+5am
(5am:test expression-test
          (5am:is (funcall #'expression
                    (coerce "'+'/'-'" 'list) 0)))

; Sequence <- Rule (Spacing Rule)*
(defexpr sequence-expr
  (compose
    #'rule
    (zero-or-more 
      (compose
        #'spacing #'rule))))
#+5am
(5am:test sequence-expr-test
          (5am:is (funcall #'sequence-expr
                    (coerce "'+'" 'list) 0)))


; Rule <- PosLook / NegLook / Plain
(defexpr rule
         (or-expr #'pos-look #'neg-look #'plain))

; CheckId <- (upper lower+)+
(defexpr check-id
  (one-or-more 
    (compose #'upper-case (one-or-more #'lower-case))))
#+5am
(5am:test check-id-test
          (5am:is (funcall #'check-id
                    (coerce "AddExpr" 'list) 0))
          (5am:is (funcall #'check-id
                    (coerce "Helloworld" 'list) 0))
          (5am:is (eq NIL
                      (funcall #'check-id
                        (coerce "helloWorld" 'list) 0))))

; Plain <- Primary Quant?
(defexpr plain (compose #'primary (opt-expr #'quant)))
#+5am
(5am:test plain-test
          (5am:is (funcall #'plain
                    (coerce "Helloworld" 'list) 0))
          (5am:is (funcall #'plain
                    (coerce "AddExpr*" 'list) 0))
          (5am:is (eq NIL
                      (funcall #'plain
                        (coerce "helloWorld" 'list) 0)))
          (5am:is (eq NIL
                      (funcall #'plain
                        (coerce "333" 'list) 0))))

; PosLook <- '&' Primary Quant?
(defexpr pos-look
  (compose (char-literal #\&) #'primary
           (opt-expr #'quant)))

; NegLook <- '!' Primary Quant?
(defexpr neg-look
  (compose (char-literal #\!) #'primary
           (opt-expr #'quant)))

; Primary <- Simple / CheckId / '(' Expression ')'
(defexpr primary
         (or-expr
          #'simple #'check-id
          (compose
           (char-literal #\()
           #'expression
           (char-literal #\)))))
#+5am
(5am:test primary-test
          (5am:is (funcall #'primary
                    (coerce "('+'/'-')" 'list) 0)))

; ScanDef <- CheckId SP+ '<-'  SP+ Expression 
(defexpr definition
  (compose
    #'check-id
    (one-or-more (char-literal #\SP))
    (or-expr (string-expr "<-")
             (char-literal #\LEFTWARDS_ARROW))
    (one-or-more (char-literal #\SP))
    #'expression))
#+nil
(let (
      (cl-peg-yapp/peg-parser::*print-match-error* t)
      )
 (funcall #'definition
                    (coerce "AddExpr  <- ('+'/'-') Factor" 'list) 0) 
  )

#+5am
(5am:test scan-def-test
          (5am:is (funcall #'definition
                    (coerce "AddExpr  <- ('+'/'-') Factor" 'list) 0))
          (5am:is (funcall #'definition
                    (coerce "First <- [a-d]" 'list) 0))
          (5am:is (eq NIL
                      (funcall #'definition
                        (coerce "HelloWorld" 'list) 0))))
