;;;; helper functions and macros for matches

(in-package #:cl-peg-yapp/peg-parser)

(defun test-full-match (expr test-str &key parser-expr 
                             (other-value nil other-p))
  "tests for a full match for the provided
   parsing expression and test string."
  (declare (string test-str))
  (let* ((test-input (coerce test-str 'list))
         (test-value (if other-p other-value
                         (length test-input)))
         (match-node 
           (if parser-expr 
               (funcall expr test-str)
               (funcall expr test-input 0)))
         (actual-len 
           (and match-node (match-end match-node))))
    (5am:is
     (eql test-value actual-len)
     (format nil "Parsed grammar for ~a did not match.~
                  ~&Expected value/length: ~a~
                  ~&Actual: ~a" test-str test-value actual-len))))

