(in-package #:cl-peg-yapp/peg-scanner)

#+5am
(5am:in-suite scanner-suite)

(defun parse-grammar (grammar-string)
  (declare (string grammar-string))
  (parse #'cl-peg-yapp/peg-grammar:spec grammar-string))

#+nil
(let (
      (cl-peg-yapp/peg-parser::*compacted-tree* t)
      ;(cl-peg-yapp/peg-parser::*packrat-enabled* nil)
      ;(cl-peg-yapp/peg-parser::*print-match-error* t)
      )
  (parse-grammar 
    (uiop:read-file-string (pathname #p"grammars/peg-simple.peg"))))

#+nil
(spec 
  (coerce
  (uiop:read-file-string (pathname #p"grammars/example.peg"))
    'list) 0)

