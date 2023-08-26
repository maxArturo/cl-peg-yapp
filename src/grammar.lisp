;;;; peg grammar parser style implementation

(in-package #:peg-parser)

#+5am
(5am:in-suite parser-suite)

(defun char-terminal (input &key char-literal)
  "returns the car of input list if it
   is a char. otherwise NIL"
  (declare (list input))
  (let ((curr (car input)))
    (and (characterp curr) 
         (if char-literal (char= char-literal curr) T)
         (list (list curr) (cdr input)))))

(defun literal-char-terminal (literal-char)
  "special case of `char-terminal` that compares
   against a specific char."
  (lambda (input) 
    (char-terminal input :char-literal literal-char)))

(defun negate (expr)
  "implements negative-lookahead for a PEG expr.
   Function returns NIL if expression succeeds.
   Otherwise returns a tuple-list with
   NIL and intact input."
  (lambda (input)
    (declare (list input))
    (let ((ans (funcall expr input)))
      (if ans NIL
          (list (list NIL) input)))))

(defun compose (&rest exprs)
  "returns a higher order function. 
   It applies the output of each expr to the
   next until either one returns NIL, or they all
   succeed. It assumes in/output of 
   (list (list ...nodes) (list rem-input)
   This has the effect of applying parens on these
   expressions."
  
  (lambda (input)
    (let ((results 
     (multiple-value-list 
       (for:for 
        ((pred over exprs)
         (curr-ans = (funcall pred input))
         (expressions when curr-ans reducing (first curr-ans) :by 
           (lambda (prev-var called-form) 
             (if (first called-form)
                 (concatenate 'list prev-var called-form)
                 prev-var))))
        (print "curr ans is")
        (print curr-ans)
        (always curr-ans)
        (if (second curr-ans) (setf input (second curr-ans)))
        (returning input)))))
      (print results)
      (and (first results) (cdr results)))))


