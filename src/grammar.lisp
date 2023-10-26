;;;; peg grammar parser style implementation

;;; This file includes basic terminal definitions, as well
;;; as operators on both terminals and non-terminals.

(in-package #:peg-parser)

#+5am
(5am:in-suite parser-suite)

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

(defun literal-char-terminal (literal-char)
  "returns higher-order function that tests
   against a specific char."
  (lambda (input) 
    (and (characterp (car input)) 
      (char= literal-char (car input))
         (list :result (list (make-terminal :value (car input)))  
               :remainder (cdr input)))))

(defun negate (expr)
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
              ; compact emtpy success nodes
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

(defun one-or-more (expr)
  "will apply expr greedily. Must succeed at least
   once"
  (lambda (input)
    (funcall 
      (compose expr (funcall #'zero-or-more expr))
      input)))

(defun optional (expr)
  "applies expr zero or one times"
  (lambda (input)
    (let ((result (funcall expr input)))
     (or result (list :result '(:empty-success) :remainder input)))))

(defun or-expr (&rest exprs)
  "attempts exprs, until one succeeds.
   Returns NIL otherwise"
  (lambda (input)
    (for:for 
      ((expr over exprs)
       (curr-ans = (funcall expr input)))
         (until curr-ans)
         (returning curr-ans))))

