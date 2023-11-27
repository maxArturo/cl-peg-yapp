(in-package #:peg-scanner)

#+5am
(5am:in-suite scanner-suite)

(defun parse-grammar (&optional (filename (pathname #p"grammars/peg-simple.peg")))
  "loads the input grammar file (*.peg)"
  (let ((grammar-str (coerce (uiop:read-file-string filename) 'list)))
    (spec grammar-str 0)))
#+nil
(let (
      (PEG-PARSER::*compacted-tree* t)
      ;(peg-parser::*print-match-error* t)
      )
  (parse-grammar (pathname #p"grammars/math.peg")))

