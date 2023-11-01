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

(defun upper-case ()
  "parses an upper-case letter in the ascii range
   A-Z."
  (parent-expr 
    (char-range-terminal #\A #\Z)
    :upper-case))
(5am:test upper-case-test
  (5am:is 
    (funcall (upper-case)
             (coerce "First" 'list)))
  (5am:is (eq NIL
    (funcall (upper-case)
             (coerce "jigaro" 'list)))))

(defun lower-case ()
  "parses an upper-case letter in the ascii range
   a-z."
  (parent-expr 
    (char-range-terminal #\a #\z) 
    :lower-case))
(5am:test lower-case-test
  (5am:is 
    (funcall (lower-case)
             (coerce "first" 'list)))
  (5am:is (eq NIL
    (funcall (lower-case)
             (coerce "Jigaro" 'list)))))

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

(defun or-literal ()
  "Parses against a regex-style set of options 
   including ranges, e.g. [A-Za-z0-9]"
  (lambda (input) 
    (destructuring-bind 
      (&key result remainder)
      (funcall
        (compose (literal-char-terminal #\[)
                 
                 (literal-char-terminal #\]))
        input)
      (and 
        result 
        (list :result 
              ; TODO make this the actual terminals
          (list (make-parent 
            :kind parent-kind
            :children result)) 
          :remainder
          remainder)))))
