(in-package #:cl-peg-yapp/peg-scanner)

#+5am
(5am:in-suite scanner-suite)

#+5am
(5am:def-suite* scanner-generator-suite :in scanner-suite)

(defun parse-grammar (file)
  (parse #'cl-peg-yapp/peg-grammar:pattern
         (uiop:read-file-string file)))

#+nil
(generate (parse-grammar #p"src/tests/grammars/markdown.peg"))

#+5am
(5am:test generator-scanner-test
  (mapcar 
    (lambda (str)
      (test-full-match 
       (generate 
         (parse-grammar #p"src/tests/grammars/date.peg"))
       str :parser-expr t))
    '("Fri Jun 17 03:50:56 PDT 2011"
      "2010-10-26 10:00:53.360"))
  (test-full-match
    (generate 
      (parse-grammar #p"src/tests/grammars/markdown.peg"))
    "**hello**" :parser-expr t))

#+nil
(funcall 
 (generate 
  (parse-grammar #p"src/tests/grammars/date.peg"))
 "2010-10-26 10:00:53.360")

