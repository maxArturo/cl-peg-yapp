(in-package #:peg-scanner)

#+5am
(5am:in-suite scanner-suite)

(defun parse-grammar (grammar-string)
  (declare (string grammar-string))
  (parse #'peg-grammar:spec grammar-string))

#+nil
(let (
      (PEG-PARSER::*compacted-tree* t)
      ;(peg-parser::*packrat-enabled* nil)
      ;(peg-parser::*print-match-error* t)
      )
  (parse-grammar 
    (uiop:read-file-string (pathname #p"grammars/example.peg"))))

#+nil
(spec 
  (coerce
  (uiop:read-file-string (pathname #p"grammars/example.peg"))
    'list) 0)

