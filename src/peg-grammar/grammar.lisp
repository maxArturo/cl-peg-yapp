;;; This file has the top level definitions
;;; for a grammar.

(in-package #:cl-peg-yapp/peg-grammar)

#+5am
(5am:def-suite* full-spec-suite :in grammar-suite)

; Spec       <-- ComEndLine*
;                (Definition ComEndLine*)+
(defexpr spec
 (compose
  (zero-or-more #'comment-endline)
  (zero-or-more #'end-line)
  (one-or-more
   (compose
    #'definition
    (zero-or-more #'comment-endline)))
  (zero-or-more #'end-line)))
#+5am
(5am:test spec-test
  (5am:is 
    (funcall #'spec (coerce
"# this is a test spec
# it's weird
# but it's valid
Word <- Letter+ # with comments too! 
Letter <- [A-Za-z] " 'list) 0))

  (5am:is (funcall #'spec (coerce
#?|\
# this spec was taken off of wikipedia.
# see https://en.wikipedia.org/wiki/Parsing_expression_grammar#Examples
# for details.


Expr    ← Sum
Sum     ← Product (('+' / '-') Product)*
Product ← Power (('*' / '/') Power)*
Power   ← Value ('^' Power)?
Value   ← [0-9]+ / '(' Expr ')'
|
                          'list) 0))
          (5am:is (eq NIL
                      (funcall #'spec
                        (coerce "HelloWorld" 'list) 0))))

