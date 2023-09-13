;;;; peg grammar parser style implementation

(in-package #:peg-parser)

#+5am
(5am:in-suite parser-suite)

(defun char-terminal (input)
  "returns the car of input list if it
   is a char. otherwise NIL"
  (declare (list input))
  (let ((curr (car input)))
    (and (characterp curr) 
         (list :result '(curr) :remainder (cdr input)))))

(defun literal-char-terminal (literal-char)
  "returns higher-order function that tests
   against a specific char."
  (lambda (input) 
    (and (characterp (car input)) 
      (char= literal-char (car input))
         (list :result (list (car input))  
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
         (list :result '(NIL) :remainder input)))))

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
               (concatenate 'list prev-result curr-result))))
         ; (print "curr ans is")
         ; (print curr-ans)
         (always curr-ans)
         (setf input remainder)
         (returning input)))))
      ;( print results)
      (and (first results) 
           (list :result (third results) 
                 :remainder (second results))))))


