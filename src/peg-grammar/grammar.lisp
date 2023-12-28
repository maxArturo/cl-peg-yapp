;;; This file has the top level definitions
;;; for a grammar.

(in-package #:cl-peg-yapp/peg-grammar)

(interpol:enable-interpol-syntax)

#+5am
(5am:def-suite* full-grammar-suite :in grammar-suite)

; Pattern         <- Exp !.
(defexpr pattern
         (compose
           #'expression
           (negative-lookahead #'any-char)))
#+5am
(5am:test pattern-test
  (test-full-match
    #'pattern
    #?"\
Word <- Letter+ # with comments too!
Letter <- [A-Za-z]")
   (test-input
     #'pattern
     #?"\
Expr    ← 
  Sum

Sum     ← Product ((u0043 / '-') Product)*

Product ← Power (('*' / '/') Power)*

Power   ← Value ('^' Power)?

Value   ← !'tacos'{1,583}
          [`0-9]+ 
          / '(' Expr ')'")
     (test-full-match #'grammar "HelloWorld" :other-value nil))

; Grammar         <- Definition+
(defexpr grammar
         (one-or-more #'definition))
#+5am
(5am:test grammar-test
  (test-full-match
    #'grammar 
    #?"\
Word <- Letter+ # with comments too!
Letter <- [A-Za-z]")
   (test-input
     #'grammar 
     #?"\
Expr    ← 
  Sum

Sum     ← Product (('+' / '-') Product)*

Product ← Power (('*' / '/') Power)*

Power   ← Value ('^' Power)?

Value   ← 
          [0-9]+ 
          / '(' Expr ')'")
     (test-full-match #'grammar "HelloWorld" :other-value nil))

#+5am
 (5am:test e2e-test
  (let* ((filenames 
           (mapcar (lambda (f) (enough-namestring f (uiop:getcwd))) 
                   (directory #p"tests/grammars/*.peg"))))
    (mapcar 
      (lambda (f)
        (let* ((test-grammar-str
                 (uiop:read-file-string (pathname f)))
               (test-len (length test-grammar-str))
               (match-node (parse #'grammar test-grammar-str))
               (actual-len (and match-node (match-end match-node))))
          (5am:is
           (eql test-len actual-len)
           (format nil "Parsed grammar for ~a did not match.~
                        ~&Expected length: ~a~
                        ~&Actual: ~a" f test-len actual-len)))) 
                        filenames)))

; Exp             <- Spacing (Alternative / Grammar)
(defexpr expression
         (compose
           #'spacing
           (or-expr #'alternative #'grammar)))
#+5am
(5am:test expression-test
  (test-input
     #'expression
     #?"\
# This is the main comment area.
# This could be a useful comment too.

Expr    ← 
  Sum

Sum     ← Product (('+' / '-') Product)*

Product ← Power (('*' / '/') Power)*

Power   ← Value ('^' Power)?

Value   ← 
          [0-9]+ 
          / '(' Expr ')'")
  (test-full-match #'expression "'+'/'-'")
  (test-full-match #'expression "((alphanum '-' alphanum) / alphanum)")
  (test-full-match #'expression "alphanum / numeric")
  (test-full-match #'expression #?"\
Modifier* (ClassDeclaration
			     / EnumDeclaration
			     / InterfaceDeclaration
			     / AnnotationTypeDeclaration) / SEMI")
  (test-full-match #'expression "([_-*] AnySpace){3,} EndLine"))

 
