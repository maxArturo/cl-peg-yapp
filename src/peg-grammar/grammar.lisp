;;; This file has the top level definitions
;;; for a grammar.

(in-package #:cl-peg-yapp/peg-grammar)

#+5am
(5am:def-suite* full-grammar-suite :in grammar-suite)

; Grammar <- ComEndLine* (Definition ComEndLine*)+
(defexpr grammar
         (compose
           (zero-or-more #'comment-endline)
           (one-or-more
             (compose
               #'definition
               (zero-or-more #'spacing)))))
#+5am
(5am:test grammar-test
  (5am:is 
   (funcall 
     #'grammar
     (coerce
"# this is a test grammar
# it's weird
# but it's valid

Word <- Letter+ # with comments too! 
Letter <- [A-Za-z] " 'list) 0))

#+5am
(5am:is 
 (grammar
   (coerce
     #?"
     # this grammar was taken off of wikipedia.
# see https://en.wikipedia.org/wiki/Parsing_expression_grammar#Examples
# for details.


Expr    ← Sum
Sum     ← Product (('+' / '-') Product)*
Product ← Power (('*' / '/') Power)*
Power   ← Value ('^' Power)?
Value   ← [0-9]+ / '(' Expr ')'"
     'list) 0))
        (5am:is 
         (eq NIL (grammar (coerce "HelloWorld" 'list) 0))))

