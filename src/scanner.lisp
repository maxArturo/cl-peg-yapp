(in-package #:cl-peg-yapp/peg-scanner)

#+5am
(5am:in-suite scanner-suite)

#+5am
(5am:def-suite* scanner-generator-suite :in scanner-suite)

(defun parse-grammar (file)
  (parse #'cl-peg-yapp/peg-grammar:grammar
         (uiop:read-file-string file)))
#+5am
(5am:test generator-scanner-test
  (test-full-match
    (generate 
      (parse-grammar #p"grammars/math.peg"))
    "3 +     19 / (3)"
    :parser-expr t)
  (mapcar 
    (lambda (str)
      (test-full-match 
        (generate 
          (parse-grammar #p"grammars/date.peg"))
        str :parser-expr t))
    '("Fri Jun 17 03:50:56 PDT 2011"
      "2010-10-26 10:00:53.360")))

