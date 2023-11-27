;;; This file includes basic terminal definitions, as well
;;; as operators on both terminals and non-terminals as described
;;; for PEG expressions. They can be used for implementing
;;; a generated parser, including the actual PEG parser that lives
;;; in `src/peg-grammar`. 

(in-package #:peg-parser)

#+5am
(5am:def-suite* base-suite :in parser-suite)

(defmacro defpattern (pattern-kind expr-lambda &optional exp-p)
  "defines an expression pattern, optionally
   interpreted as a meaningful expression node."
  (let ((pattern-keyword (intern (symbol-name pattern-kind) "KEYWORD")))
    `(defun ,pattern-kind (input index)
       (declare (list input) (fixnum index))
       (let* ((result (funcall ,expr-lambda input index))
              (result-match 
                (and result
                     (new-match 
                       input index (match-end result) result
                       ,(and exp-p pattern-keyword)))))
         (if (and *print-match-error* (not result-match))
             (format t "Did not match for ~a in index: ~a"
                     ,pattern-keyword index))
         (compact-match result-match)))))

(defmacro defexpr (parent-kind expr-lambda)
  "defines the main lambda thunk for applying an 
   expression. If successful, returns a match.
   Returns nil if not valid"
  `(defpattern ,parent-kind ,expr-lambda t))

; this is effectively equivalent to 'unicode'
; in a PEG.
(defun any-char (input i)
  "returns the car of input list if it
   is a char. otherwise NIL"
  (declare (list input) (fixnum i))
  (let ((curr (nth i input))
        (start i)
        (end (+ i 1)))
    (and (characterp curr)
         (new-match input start end))))
#+5am
(5am:test any-char-test
  (5am:is
   (any-char (coerce "funky" 'list) 4))
  (5am:is (eq NIL (any-char '() 3)))
  (5am:is (eq NIL
              (any-char (coerce "funky" 'list) 5))))

(defun char-literal (literal-char)
  "returns higher-order function that tests
   against a specific char."
  (lambda (input i)
    (declare (list input) (fixnum i))
    (let ((curr (nth i input))
          (start i)
          (end (+ i 1)))
      (and (characterp curr)
           (char= literal-char curr)
           (new-match input start end)))))
#+5am
(5am:test char-literal-test
  (5am:is
   (funcall (char-literal #\f)
            (coerce "funky" 'list) 0))
  (5am:is
   (eq NIL
       (funcall (char-literal #\f)
                (coerce "tunky" 'list) 0)))
  (5am:is
   (eq NIL
       (funcall (char-literal #\f)
                (coerce "funky" 'list) 1))))

(defun char-range (start-char end-char)
  "Parses against a code-point range of two given chars.
   Roughly equivalent to a regex range, e.g. [a-z]"
  (lambda (input i)
    (declare (list input) (fixnum i))
    (let ((curr (nth i input))
          (start i)
          (end (+ i 1)))
      (and
        (characterp curr)
        (char<= start-char curr) (char>= end-char curr)
        (new-match input start end)))))
#+5am
(5am:test char-range-test
  (5am:is (funcall (char-range #\f #\i)
                   (coerce "hunky" 'list) 0))
  (5am:is (funcall (char-range #\u #\z)
                   (coerce "hunky" 'list) 4))
  (5am:is (eq NIL
              (funcall (char-range #\f #\i)
                       (coerce "tunky" 'list) 0))))

(defun positive-lookahead (expr)
  "implements positive lookahead for a PEG 
   expr (&). Function returns NIL if expression 
   fails, otherwise returns an empty match."
  (lambda (input i)
    (declare (list input) (fixnum i))
    (let ((ans (funcall expr input i)))
      (and ans (empty-match input i)))))

(defun negative-lookahead (expr)
  "implements negative-lookahead for a PEG expr
   (i.e. `!`). Function returns NIL if expression 
   succeeds, otherwise returns an empty match."
  (lambda (input i)
    (declare (list input) (fixnum i))
    (let ((ans (funcall expr input i)))
      (and (not ans) (empty-match input i)))))
#+5am
(5am:test negative-lookahead-test
  (5am:is
   (funcall (negative-lookahead (char-literal #\i))
            (coerce "figaro" 'list) 3))
  (5am:is
   (eq nil (funcall (negative-lookahead (char-literal #\i))
                    (coerce "figaro" 'list) 1))))

(defun compose (&rest exprs)
  "It applies the output of each expr to the
   next until either one returns NIL, or they all
   succeed. This has the effect of applying parens on these
   expressions."
  (lambda (input i)
    (declare (list input) (fixnum i))
    (let*
      ((start i)
       (res (multiple-value-list 
              (for:for
                ((expr over exprs)
                 (curr-ans = (funcall expr input i))
                 (curr-results collecting curr-ans))
                (always curr-ans)
                (setf i (match-end curr-ans)))))
       (successful (first res))
       (children (second res)))
      (and successful
           (new-match input start i 
                      (or (and (listp children) children) 
                          (list children)))))))
#+5am
(5am:test compose-test
  (5am:is
   (funcall
     (compose
       (char-literal #\f)
       (char-literal #\i)
       (char-literal #\g)
       (char-literal #\a))
     (coerce "figar" 'list) 0))
  (5am:is (eq NIL
              (funcall
                (compose
                  (char-literal #\i)
                  (char-literal #\g)
                  (char-literal #\a))
                (coerce "figar" 'list) 0))))

(defun times (expr n)
  "Applies expr n times, equivalent to
   `SomeExpr`{n} in PEG notation."
  (lambda (input index)
    (declare (list input) (fixnum index))
    (let ((start index)
          (results
            (multiple-value-list (for:for
                                   ((i repeat n)
                                    (curr-ans = (funcall expr input index))
                                    (curr-results collecting curr-ans))
                                   (always curr-ans)
                                   (setf index (match-end curr-ans))))))
      (and (first results)
           (new-match input start index (second results))))))
#+5am
(5am:test times-test
  (5am:is
   (funcall
     (times #'any-char 5)
     (coerce "figar" 'list) 0))
  (5am:is (eq NIL
              (funcall
                (times #'any-char 6)
                (coerce "figar" 'list) 0))))

(defun zero-or-more (expr)
  "applies expr greedily, and never fails.
   Equivalent to the star (*) operator in 
   PEG notation."
  (lambda (input index)
    (declare (list input) (fixnum index))
    (let ((start index)
          (results (for:for
                     ((curr-ans = (funcall expr input index))
                      (curr-results when curr-ans collecting curr-ans))
                     (while curr-ans)
                     (setf index (match-end curr-ans)))))
      (or 
        (and results
               (new-match input start index results))
          (empty-match input index)))))
#+5am
(5am:test zero-or-more-test
  (5am:is (equalp (empty-match (coerce "booyah" 'list) 0)
                  (funcall (zero-or-more (char-literal #\f))
                           (coerce "booyah" 'list) 0)))
  (5am:is (equalp (new-match (coerce "hello" 'list) 0 5)
                  (funcall
                    (zero-or-more
                      #'any-char)
                    (coerce "hello" 'list) 0))))

(defun one-or-more (expr)
  "will apply expr greedily. Must succeed at least
   once. Equivalent to the plus (+) operator in 
   PEG notation."
  (lambda (input index)
    (declare (list input) (fixnum index))
    (let ((res (funcall (compose expr (zero-or-more expr))
             input index)))
      (and res
           (new-match input index (match-end res) res)))))
#+5am
(5am:test one-or-more-test
  (5am:is (eq NIL
              (funcall
                (one-or-more
                  (char-literal #\f))
                (coerce "igaro" 'list) 0)))
  (5am:is 
   (funcall
     (one-or-more
       (char-literal #\f))
     (coerce "figaro" 'list) 0))
  (5am:is 
   (funcall
     (one-or-more
       #'any-char)
     (coerce "figaro" 'list) 0)))

(defun opt-expr (expr)
  "either parses with `expr` or returns an empty match.
   Equivalent to the (?) modifier in PEG notation."
  (lambda (input index)
    (declare (list input) (fixnum index))
    (let ((result (funcall expr input index)))
      (or 
        (and result 
             (new-match input index (match-end result) result))
        (empty-match input index)))))
#+5am
(5am:test opt-expr-test
  (5am:is (equalp (empty-match (coerce "jigaro" 'list) 0)
                  (funcall (opt-expr (char-literal #\f))
                           (coerce "jigaro" 'list) 0)))
  (5am:is (equalp (new-match 
                    (coerce "figaro" 'list) 0 1 
                    (list (new-match (coerce "figaro" 'list) 0 1))) 
                  (funcall (opt-expr (char-literal #\f))
                           (coerce "figaro" 'list) 0))))

(defun or-expr (&rest exprs)
  "attempts exprs, until one succeeds.
   Returns NIL otherwise. Equivalent to the OR
   (/) operator in PEG notation."
  (lambda (input index)
    (declare (list input) (fixnum index))
    (let ((res (for:for
                 ((expr over exprs)
                  (curr-ans = (funcall expr input index)))
                 (until curr-ans)
                 (returning curr-ans))))
      (and res
           (new-match input index (match-end res) res)))))
#+5am
(5am:test or-expr-test
  (5am:is
   (funcall
     (or-expr
       (char-literal #\f)
       (char-literal #\i)
       (char-literal #\g)
       (char-literal #\a))
     (coerce "arigar" 'list) 0))
  (5am:is
   (funcall
     (or-expr
       (char-literal #\f)
       (char-literal #\i)
       (one-or-more #'any-char)
       (char-literal #\g))
     (coerce "arigar" 'list) 0))
  (5am:is (eq NIL
              (funcall
                (or-expr
                  (char-literal #\i)
                  (char-literal #\g)
                  (char-literal #\a))
                (coerce "rigar" 'list) 0))))

(defun string-expr (input)
  "parses a provided string"
  (declare (string input))
  (let ((check (coerce input 'list)))
    (apply #'compose
           (mapcar (lambda (el) (char-literal el))
                   check))))
#+5am
(5am:test string-expr-test
  (5am:is (funcall (string-expr "hello")
                   (coerce "hello world" 'list) 0))
  (5am:is (eq NIL (funcall (string-expr "hello")
                           (coerce "hella" 'list) 0))))

