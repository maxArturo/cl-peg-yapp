;;;; parser for actual peg grammar file

;;; Here we include PEG non terminals which will be
;;; considered parent nodes.

(in-package #:peg-grammar)

#+5am
(5am:in-suite grammar-suite)

(defun expression-id ()
  "parses an expression identifier in PEG notation"
  (parent-expr
    (one-or-more
      (compose (upper-case) (one-or-more (lower-case))))
    :expression-id))
#+5am
(5am:test expression-id-test
  (5am:is 
    (funcall 
      (expression-id)
      (coerce "TheThing" 'list)))
  (5am:is (eq NIL
    (funcall 
      (expression-id)
      (coerce "33DaThing" 'list))))
  (5am:is (eq NIL
    (funcall 
      (expression-id)
      (coerce "dathing" 'list)))))

(defun expression ()
  "parses an expression rule in PEG notation"
  (parent-expr
    (one-or-more
      (compose (upper-case) (one-or-more (lower-case))))
    :expression-id))

(defun expression-def ()
  "Evaluates an expression definition"
  (parent-expr 
    (compose 
      (zero-or-more 
        (literal-char-terminal #\space))
      (expression-id)
      (one-or-more
        (literal-char-terminal #\space))
      (literal-char-terminal #\<)
      (literal-char-terminal #\-)
      (one-or-more
        (literal-char-terminal #\space))
      )
    :expression))
#+5am
(5am:test expression-test
  (5am:is 
    (funcall 
      (expression)
      (coerce "TestExpr => " 'list)))
  (5am:is (eq NIL
    (funcall 
      (expression-id)
      (coerce "33DaThing" 'list))))
  (5am:is (eq NIL
    (funcall 
      (expression-id)
      (coerce "dathing" 'list)))))

(defun char-range-literal ()
  "parses a literal range of chars, e.g. 'a-z' or 
   '0-9'. Ranges must have a dash and must not start
   with a dash."
  (parent-expr (compose 
    (negative-lookahead (literal-char-terminal #\-))
    (char-terminal)
    (literal-char-terminal #\-)
    (negative-lookahead (literal-char-terminal #\-))
    (char-terminal))
    :char-range-literal))
#+5am
(5am:test char-range-literal-test
  (5am:is 
    (funcall 
      (char-range-literal)
      (coerce "a-zforeva" 'list)))
  (5am:is (eq NIL
    (funcall 
      (char-range-literal)
      (coerce "a--zforeva" 'list))))
  (5am:is (eq NIL
    (funcall 
      (char-range-literal)
      (coerce "-a--zforeva" 'list))))
  )

