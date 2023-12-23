;;; This file has the top level definitions
;;; for a grammar.

(in-package #:cl-peg-yapp/peg-grammar)

(interpol:enable-interpol-syntax)

#+5am
(5am:def-suite* full-grammar-suite :in grammar-suite)

; Grammar <- ComEndLine* (Definition ComEndLine*)+
(defexpr grammar
         (compose
           (zero-or-more #'comment-endline)
           (one-or-more
             (compose
               #'definition
               (zero-or-more #'comment-endline)))))
#+5am
(5am:test grammar-test
  (test-full-match 
    #'grammar 
    #?"\
# this is a test grammar
# it's weird
# but it's valid

Word <- Letter+ # with comments too!
Letter <- [A-Za-z]")
   (test-full-match 
     #'grammar 
     #?"\
# this grammar was taken off of wikipedia.
# see https://en.wikipedia.org/wiki/Parsing_expression_grammar#Examples
# for details.

Expr    ← Sum
Sum     ← Product (('+' / '-') Product)*
Product ← Power (('*' / '/') Power)*
Power   ← Value ('^' Power)?
Value   ← [0-9]+ / '(' Expr ')'")
     (test-full-match #'grammar "HelloWorld" :other-value nil))

#+5am
 (5am:test e2e-test
  (let* ((filenames 
           (mapcar (lambda (f) (enough-namestring f (uiop:getcwd))) 
                   (directory #p"grammars/*.peg"))))
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

