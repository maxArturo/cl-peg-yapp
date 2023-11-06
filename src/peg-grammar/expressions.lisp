;;;; parser for actual peg grammar file

;;; Here we include PEG non terminals which will be
;;; considered parent nodes.

(in-package #:peg-grammar)

#+5am
(5am:in-suite grammar-suite)

(defun line-end ()
  "parses all combinations of a line ending"
  (parent-expr 
    (or-expr 
        (literal-char-terminal #\cr) 
        (literal-char-terminal #\lf) 
        (compose
          (literal-char-terminal #\cr) 
          (literal-char-terminal #\lf)))
    :line-end))
(5am:test line-end-test
  (5am:is 
    (funcall (line-end)
             (list #\CR)))
  (5am:is (eq NIL
    (funcall (line-end)
             (coerce "   jigaro" 'list)))))

(defun comment-line ()
  "parses a full comment line"
  (parent-expr 
    (compose 
      (zero-or-more 
        (literal-char-terminal #\space))
      (literal-char-terminal #\#) 
      (zero-or-more 
        (compose
          (negative-lookahead (line-end))
          (char-terminal))))
    :comment-line))
(5am:test comment-line-test
  (5am:is 
    (funcall (comment-line) 
             (coerce "   ### jigaro" 'list)))
  (5am:is (eq NIL
    (funcall (comment-line) 
             (coerce "jigaro" 'list)))))

(defun comment-endline ()
  (parent-expr 
    (compose (comment-line) (line-end))
    :comment-endline))
(5am:test comment-endline-test
  (5am:is 
    (funcall (comment-endline) 
             (list #\# #\Newline)))
  (5am:is (eq NIL
    (funcall (comment-endline) 
             (coerce "jigaro" 'list)))))

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

(defun string-literal ()
  "represents a single-quoted string, e.g. 'hello'"
  (parent-expr
    (compose
      (literal-char-terminal #\') 
      (one-or-more
        (compose
          (negative-lookahead (literal-char-terminal #\')) 
          (char-terminal)))
      (literal-char-terminal #\'))
    :string-literal))
(5am:test string-literal-test
  (5am:is 
    (funcall 
      (string-literal)
      (coerce "'something'" 'list)))
  (5am:is (eq NIL
    (funcall 
      (string-literal)
      (coerce "'33DaThing" 'list)))))

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

(defun or-literal ()
  "Parses against a regex-style set of char options,
   including ranges, e.g. [A-Za-z0-9]"
  (parent-expr
    (compose 
      (literal-char-terminal #\[)
      (one-or-more
        (compose
          (negative-lookahead (literal-char-terminal #\]))
          (or-expr 
            (char-range-literal)
            (char-terminal))))
      (literal-char-terminal #\]))
    :or-literal))
#+5am
(5am:test or-literal-test
  (5am:is 
    (funcall 
      (or-literal)
      (coerce "[`A-Za-z0-9_]" 'list)))
  (5am:is (eq NIL
    (funcall 
      (or-literal)
      (coerce "[]|`A-Za-z0-9_" 'list)))))

(defun simple-expr ()
  "an expression that is either a codepoint literal, 
   a range or a string."
  (parent-expr
    (or-expr 
      (or-literal)
      (string-literal)
      (compose 
        (literal-char-terminal #\u)
        (char-terminal)))
    :simple-expr))
#+5am
(5am:test simple-expr-test
  (5am:is 
    (funcall 
      (simple-expr)
      (coerce "[`A-Za-z0-9_]" 'list)))
  (5am:is (eq NIL
    (funcall 
      (or-literal)
      (coerce "[]|`A-Za-z0-9_" 'list)))))

(defun primary-expr ()
  ""
  )

(defun neg-look-expr ()
  "implements a negative lookahead in peg, e.g.
   `!'abc'`."
  (parent-expr
    (compose 
      (literal-char-terminal #\!)

      )
    :neg-look-expr))

