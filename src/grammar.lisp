(in-package #:peg-parser)

#+5am
(5am:in-suite parser-suite)

(defvar *input-buffer*)

(defun load-config (&key (filename "input.peg")) 
  "loads the input grammar file (*.peg)"
  (setf *input-buffer* (uiop:read-file-lines filename)))

;; sample usage
;; (load-config :filename "/Users/max/Developer/hard_lisp/site_generator/site.toml")


(defmacro char-kind (name &rest forms)
  "generates kinds of character function
   tests"
  `(defun ,name (in)
     (and (characterp in)
          ,@(or forms '(T)) 
          )))

;; char definitions
;; note that these definitions are much wider and more permissive
;; than those in the spec

(char-kind chr)

(char-kind unipoint (<= 32 (char-code in) 126))

(char-kind widepoint (<= 255 (char-code in)))

(char-kind space-chr (= 32 (char-code in)))

(char-kind endline (find in '(#\cr #\lf)))

(let ((line-end-str (coerce (list #\cr #\lf) 'string))) 
  (defun line-end (in) 
    (or (eql in line-end-str) (endline in))))

(char-kind lower (<= 97 (char-code in) 122))

(char-kind upper (<= 65 (char-code in) 90))

;;; productions
;; what is a line
;; Line <- (!EndLine Char)* EndLine 
;; our objective is to compose the above dynamically at will

;; creating predicates
;; predicates look ahead, but do not consume input. 
;; if the shape is '(expression remaining-input) and you want to 
;; apply a predicate, then you
;; - eval the expression on your input
;; - if not NIL, proceed but do not forward the remaining input. use the 
;;   original input instead
;; - else return nil 
;; lookahead predicates do not return structs (as they do not consume
; input), they simply return either the same input *or* NIL.

(defun star-operator (expr input)
  (star-operator-helper expr (list) input))

(defun star-operator-helper (expr predicate-list input)
  (trivia:match (funcall expr input) 
      ((list pred-match-list rem-input)
       (star-operator-helper 
         expr (append predicate-list pred-match-list) rem-input))
      (nil
        (list predicate-list input))))


(defun negative-predicate (expr input)
  "applies a negative lookahead expression on the input.
   returns NIL if expr succeeds, else returns `input` unchanged."
  (declare (list input))
  (cond ((funcall expr input) input) 
        ('T NIL)))

#+5am
(5am:test negative-predicate-test
  (for:for 
    (((&key expected input) in 
        (list
          (list 
            :expected '(T a b)
            :input (list (lambda (x) x) '(T a b)))
          (list 
            :expected NIL
            :input (list (lambda (x) NIL) '(NIL))))))
    (trivia:match input
      ((list expr arg)
       (5am:is (eql expected (negative-predicate expr arg)))))))

(defun char-production (input) 
  "verifies a single char in the next input.
   returns both a struct and the remaining input to be consumed"
  (declare (list input))
  (let 
    ((val (car input))) 
    (and (characterp val)
         (list 
           (list (make-expression :kind :char :value (list val)))
           (cdr input)))))

#+5am
(5am:test char-production-test
  (for:for 
    (((&key expected input) in 
        (list
          (list 
            :expected (make-expression :kind :char :value '(#\a))  
            :input '(#\a #\y #\o))
          (list 
            :expected NIL
            :input '()))))
    (trivia:match expected
      ((expression kind value)
       (let ((res (char-production input)))
                (5am:is (equal (expression-kind (car res)) kind))
                (5am:is (equal (expression-value (car res)) value))))
      ((T) 
       (5am:is (null (char-production input)))))))

(defun endline-production (input)
  "returns both a struct and the remaining input to be consumed"
  (declare (list input))
  (trivia:match input 
    ((trivia:guard (list* curr next _)
            (and (eql curr #\CR) (eql next #\LF)))
     (list (make-expression 
                   :kind :endline 
                   :value (list curr next)) (cddr input)))
    ((trivia:guard (list* curr _)
            (or (eql curr #\CR) (eql curr #\LF)))

     (list (make-expression 
                   :kind :endline 
                   :value (list curr)) (cdr input)))))

#+5am
(5am:test endline-production-test
  (for:for 
    (((&key expected input) in 
        (list
          (list 
            :expected (make-expression :kind :endline :value '(#\CR))  
            :input '( #\CR #\a #\y #\o))
          (list 
            :expected NIL
            :input (coerce "something else" 'list))
          (list 
            :expected (make-expression :kind :endline :value '(#\CR #\LF))  
            :input '(#\CR #\LF #\a #\y #\o))
          (list 
            :expected (make-expression :kind :endline :value '(#\LF))
            :input '( #\LF #\a #\y #\o)))))
    (trivia:match expected
      ((expression kind value)
       (let ((res (endline-production input)))
                (5am:is (equal (expression-kind (car res)) kind))
                (5am:is (equal (expression-value (car res)) value))))
      ((T) 
       (5am:is (null (endline-production input)))))))


