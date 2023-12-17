(in-package #:cl-peg-yapp/peg-scanner)

#+5am
(5am:in-suite scanner-suite)

(defun parse-grammar (file)
  (parse #'cl-peg-yapp/peg-grammar:grammar
         (uiop:read-file-string file)))
;#+5am
; (5am:test 
;  scanner-e2e-test
;  (let* ((filenames 
;           (mapcar (lambda (f) (enough-namestring f (uiop:getcwd))) 
;                   (directory #p"grammars/*.peg"))))
;    (mapcar 
;      (lambda (f)
;        (let* ((test-grammar-str
;                 (uiop:read-file-string (pathname f)))
;               (test-len (length test-grammar-str))
;               (match-node (parse #'cl-peg-yapp/peg-grammar:grammar test-grammar-str))
;               (actual-len (and match-node (match-end match-node))))
;          (5am:is
;           (eql test-len actual-len)
;           (format nil "Parsed grammar for ~a did not match.~
;                        ~&Expected length: ~a~
;                        ~&Actual: ~a" f test-len actual-len)))) 
;                        filenames)))

