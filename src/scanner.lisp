(in-package #:cl-peg-yapp/peg-scanner)

#+5am
(5am:in-suite scanner-suite)

(defun parse-grammar (file)
  (parse #'cl-peg-yapp/peg-grammar:grammar
         (uiop:read-file-string file)))

#+nil
(parse-grammar (pathname #p"grammars/basic.peg"))
