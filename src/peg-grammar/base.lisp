;;;; peg grammar parser style implementation

;;; This file includes basic terminal definitions, as well
;;; as operators on both terminals and non-terminals.

(in-package #:peg-grammar)

#+5am
(5am:in-suite grammar-suite)

(defun char-terminal ()
  "returns the car of input list if it
   is a char. otherwise NIL"
  (lambda (input)
    (declare (list input))
    (let ((curr (car input)))
      (and (characterp curr) 
        (list 
          :result 
          (list (make-terminal :value curr)) 
          :remainder (cdr input))))))
#+5am
(5am:test char-terminal-test
    (5am:is (funcall (char-terminal) (coerce "funky" 'list)))
    (5am:is (eq NIL (funcall (char-terminal) '()))))

(defun literal-char-terminal (literal-char)
  "returns higher-order function that tests
   against a specific char."
  (lambda (input) 
    (and (characterp (car input)) 
      (char= literal-char (car input))
         (list :result (list (make-terminal :value (car input)))  
               :remainder (cdr input)))))
#+5am
(5am:test literal-char-terminal-test
    (5am:is (funcall (literal-char-terminal #\f) 
                     (coerce "funky" 'list)))
    (5am:is (eq NIL 
      (funcall (literal-char-terminal #\f) 
               (coerce "tunky" 'list)))))

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

(defun digit ()
  "parses a digit in the ascii range."
  (parent-expr 
    (char-range-terminal #\0 #\9) 
    :digit))
(5am:test digit-test
  (5am:is 
    (funcall (digit)
             (coerce "0bacon" 'list)))
  (5am:is (eq NIL
    (funcall (digit)
             (coerce "oneBacon" 'list)))))

(defun char-range-terminal (start-char end-char)
  "Parses against a code-point range of two given chars.
   Roughly equivalent to a regex range, e.g. [a-z]"
  (lambda (input) 
    (and 
      (characterp (car input)) 
      (char<= start-char (car input)) (char>= end-char (car input))  
      (list :result (list (make-terminal :value (car input)))  
            :remainder (cdr input)))))
#+5am
(5am:test char-range-terminal-test
    (5am:is (funcall (char-range-terminal #\f #\i) 
                     (coerce "hunky" 'list)))
    (5am:is (eq NIL 
      (funcall (char-range-terminal #\f #\i) 
               (coerce "tunky" 'list)))))

(defun positive-lookahead (expr)
  "implements positive lookahead for a PEG expr.
   Function returns NIL if expression fails.
   Otherwise returns a tuple-list with
   intact input."
  (lambda (input)
    (declare (list input))
    (let ((ans (funcall expr input)))
      (and ans
         (list 
           :result (list *empty-terminal*) 
           :remainder input)))))

(defun negative-lookahead (expr)
  "implements negative-lookahead for a PEG expr.
   Function returns NIL if expression succeeds.
   Otherwise returns a tuple-list with
   NIL and intact input."
  (lambda (input)
    (declare (list input))
    (let ((ans (funcall expr input)))
      (if ans NIL
         (list 
           :result (list *empty-terminal*) 
           :remainder input)))))
#+5am
(5am:test negative-lookahead-test
    (5am:is 
      (funcall (negative-lookahead
        (literal-char-terminal #\i))
        (coerce "figaro" 'list)))
    (5am:is (eq NIL 
      (funcall 
        (negative-lookahead
          (literal-char-terminal #\f))
        (coerce "figaro" 'list)))))

(defun compose (&rest exprs)
  "returns a higher order function. 
   It applies the output of each expr to the
   next until either one returns NIL, or they all
   succeed. It assumes in/output of 
   (list (list ...nodes) (list 'result some-res 'remainder rem-input)
   This has the effect of applying parens on these
   expressions."
  (lambda (input)
    (let ((results 
     (multiple-value-list 
       (for:for 
         ((expr over exprs)
          (curr-ans = (funcall expr input))
          ((&key result remainder) = curr-ans)
          (expressions when curr-ans reducing result :by 
            (lambda (prev-result curr-result) 
              (cond
                ((equalp *empty-terminal*
                         (first curr-result)) prev-result)
                ((equalp *empty-terminal* 
                         (first prev-result)) curr-result)
                ('t (concatenate 'list prev-result curr-result))))))
         (always curr-ans)
         (setf input remainder)
         (returning input)))))
      (and (first results) 
           (list :result (third results) 
                 :remainder (second results))))))
#+5am
(5am:test compose-test
  (5am:is 
    (funcall 
      (compose
        (literal-char-terminal #\f) 
        (literal-char-terminal #\i) 
        (literal-char-terminal #\g) 
        (literal-char-terminal #\a))
      (coerce "figar" 'list)))
  (5am:is (eq NIL
    (funcall 
      (compose
        (literal-char-terminal #\i) 
        (literal-char-terminal #\g) 
        (literal-char-terminal #\a))
      (coerce "figar" 'list)))))

(defun zero-or-more (expr)
  "applies expr greedily, and never fails"
  (lambda (input)
    (let ((results 
     (multiple-value-list (for:for 
      ((curr-ans = (funcall expr input))
       ((&key result remainder) = curr-ans)
       (expressions
         reducing result :by
         (lambda (prev-result curr-result) 
            (concatenate 'list prev-result curr-result))))
       (returning input)
       (while curr-ans)
       (setf input remainder)))))
      (list :result (or (second results) (list *empty-terminal*))
            :remainder (first results)))))
#+5am
(5am:test zero-or-more-test
  (5am:is (equalp *empty-terminal*
    (first (getf 
      (funcall 
        (zero-or-more (literal-char-terminal #\f))
        (coerce "booyah" 'list))               
      :result))))
  (5am:is (equalp (make-terminal :value #\h)
    (first (getf 
      (funcall 
        (zero-or-more
          (char-terminal))
        (coerce "hello" 'list))
      :result)))))

(defun one-or-more (expr)
  "will apply expr greedily. Must succeed at least
   once"
  (lambda (input)
    (funcall 
      (compose expr (zero-or-more expr))
      input)))
(5am:test one-or-more-test
  (5am:is (eq NIL
    (funcall 
      (one-or-more
        (literal-char-terminal #\f))
      (coerce "igaro" 'list))))
  (5am:is (funcall
    (one-or-more
      (literal-char-terminal #\f))
    (coerce "figaro" 'list))))

(defun optional (expr)
  "applies expr zero or one times"
  (lambda (input)
    (let ((result (funcall expr input)))
     (or result (list :result (list *empty-terminal*) :remainder input)))))
(5am:test optional-test
  (5am:is (equalp *empty-terminal*
    (first (getf 
      (funcall (optional (literal-char-terminal #\f))
        (coerce "jigaro" 'list))                
      :result))))
  (5am:is (equalp (make-terminal :value #\f)
    (first (getf 
      (funcall (optional (literal-char-terminal #\f))
        (coerce "figaro" 'list))                 
      :result)))))

(defun or-expr (&rest exprs)
  "attempts exprs, until one succeeds.
   Returns NIL otherwise"
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
        (literal-char-terminal #\f) 
        (literal-char-terminal #\i) 
        (literal-char-terminal #\g) 
        (literal-char-terminal #\a))
      (coerce "arigar" 'list)))
  (5am:is (eq NIL
    (funcall 
      (or-expr
        (literal-char-terminal #\i) 
        (literal-char-terminal #\g) 
        (literal-char-terminal #\a))
      (coerce "rigar" 'list)))))

(defun parent-expr (expr-lambda parent-kind)
  "creates the main lambda thunk for applying an 
   expression. If successful, returns a parent 
   struct. Returns nil if not valid"
  (lambda (input) 
    (destructuring-bind 
      (&key result remainder)
      (funcall expr-lambda
        input)
      (and 
        result 
        (list :result 
          (list (make-parent 
            :kind parent-kind
            :children result)) 
          :remainder
          remainder)))))
