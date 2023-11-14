;;; This file includes basic terminal definitions, as well
;;; as operators on both terminals and non-terminals as described
;;; for PEG expressions. They can be used for implementing
;;; a generated parser, including the actual PEG parser that lives
;;; in `src/peg-grammar`. 

(in-package #:peg-parser)

#+5am
(5am:def-suite* base-suite :in parser-suite)

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
  "implements positive lookahead for a PEG expr.
   Function returns NIL if expression fails.
   Otherwise returns a tuple-list with
   intact input."
  (lambda (input i)
    (declare (list input) (fixnum i))
    (let ((ans (funcall expr input i)))
      (and ans (empty-match input i)))))

(defun negative-lookahead (expr)
  "implements negative-lookahead for a PEG expr.
   Function returns NIL if expression succeeds.
   Otherwise returns a tuple-list with
   NIL and intact input."
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
    (funcall (negative-lookahead (char-literal #\i))
      (coerce "figaro" 'list) 1)))

(defun compose (&rest exprs)
  "It applies the output of each expr to the
   next until either one returns NIL, or they all
   succeed. This has the effect of applying parens on these
   expressions."
  (lambda (input i)
    (declare (list input) (fixnum i))
    (let 
      ((start i) 
       (res (multiple-value-list (for:for
         ((expr over exprs)
          (curr-ans = (funcall expr input i))
          (curr-results collecting curr-ans))
         (always curr-ans)
         (setf i (match-end curr-ans))))))
      (and (first res)
        (new-match input start i (second res))))))
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
     (progn 
       ;(print results)
(or (and results
               (new-match input start index results))
          (empty-match input index))
            
            ) 
      )))
#+5am
(5am:test zero-or-more-test
  (5am:is (equalp (empty-match (coerce "booyah" 'list) 0)
          (funcall (zero-or-more (char-literal #\f))
                             (coerce "booyah" 'list) 0)))
  (5am:is (equalp (new-match (coerce "hello" 'list) 0 1)
                  (first (match-children (funcall
                               (zero-or-more
                                 #'any-char)
                             (coerce "hello" 'list) 0))))))

(defun one-or-more (expr)
  "will apply expr greedily. Must succeed at least
   once. Equivalent to the plus (+) operator in 
   PEG notation."
  (lambda (input index)
    (declare (list input) (fixnum index))
    (funcall (compose expr (zero-or-more expr))
      input index)))
(5am:test one-or-more-test
  (5am:is (eq NIL
              (funcall
                  (one-or-more
                    (char-literal #\f))
                (coerce "igaro" 'list) 0)))
  (5am:is (funcall
              (one-or-more
                (char-literal #\f))
            (coerce "figaro" 'list) 0)))

(defun optional-expr (expr)
  "applies expr zero or one times. Equivalent to the
   star (*) modifier in PEG notation."
  (lambda (input)
    (let ((result (funcall expr input)))
      (or result (list :result (list *empty-terminal*) :remainder input)))))
(5am:test optional-test
  (5am:is (equalp *empty-terminal*
                  (first (getf
                           (funcall (optional-expr (char-literal #\f))
                             (coerce "jigaro" 'list))
                           :result))))
  (5am:is (equalp (make-terminal :value #\f)
                  (first (getf
                           (funcall (optional-expr (char-literal #\f))
                             (coerce "figaro" 'list))
                           :result)))))

(defun or-expr (&rest exprs)
  "attempts exprs, until one succeeds.
   Returns NIL otherwise. Equivalent to the OR
   (/) operator in PEG notation."
  (lambda (input)
    (for:for
      ((expr over exprs)
       (curr-ans = (funcall expr input)))
      (until curr-ans)
      (returning curr-ans))))
(5am:test or-expr-test
  (5am:is
    (funcall
        (or-expr
          (char-literal #\f)
          (char-literal #\i)
          (char-literal #\g)
          (char-literal #\a))
      (coerce "arigar" 'list)))
  (5am:is (eq NIL
              (funcall
                  (or-expr
                    (char-literal #\i)
                    (char-literal #\g)
                    (char-literal #\a))
                (coerce "rigar" 'list)))))

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
            (coerce "hello world" 'list)))
  (5am:is (eq NIL (funcall (string-expr "hello")
                    (coerce "hella" 'list)))))

(defmacro define-parent-expr (parent-kind expr-lambda)
  "defines the main lambda thunk for applying an 
   expression. If successful, returns a parent 
   struct. Returns nil if not valid"
  `(defun ,parent-kind (input)
     (destructuring-bind
         (&key result remainder)
         (funcall ,expr-lambda
           input)
       (and
        result
        (list :result
              (list (make-parent
                      :kind ,(intern (symbol-name parent-kind) "KEYWORD")
                      :children result
                      :match ; todo make match here
                      ))
              :remainder
              remainder)))))
