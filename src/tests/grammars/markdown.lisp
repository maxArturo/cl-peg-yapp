(in-package #:cl-peg-yapp/peg-definition-tests)

(interpol:enable-interpol-syntax)

#+5am
(5am:def-suite* markdown-grammar-suite :in grammar-definition-suite)

(defmacro with-grammar-def (grammar-def &body body)
  `(progn 
     ,@(mapcar 
         (lambda (el)
           `(5am:test ,(first el)
              (5am:is 
               (test-match-literal 
                 ',(second el)
                 (funcall 
                   (generate (parse #'pattern ,grammar-def))
                   ,(third el)))
               )))
         body)))

(defmacro md-test (test-name test-obj test-str grammar-definition)
  `(5am:test ,test-name
     (let ((grammar ,grammar-definition))
       (5am:is 
        (test-match-literal 
          ,test-obj
          (funcall
            (generate 
              (parse #'pattern grammar))
            ,test-str))))))

(defparameter *md-peg*
  (let ((md-hash (make-hash-table :test #'equal)))
    (setf (gethash "Newline" md-hash) "(u000D u000A) / u000D / u000A")
    (setf (gethash "Spacechar" md-hash) "' ' / u0009")
    (setf (gethash "Sp" md-hash) "Spacechar*")
    (setf (gethash "BlankLine" md-hash) "Sp Newline")
    md-hash))

(defun expr (expr-name)
  (let ((res (multiple-value-list (gethash expr-name *md-peg*))))
    (if (second res)
        (format nil "~a <- ~a" expr-name (first res))
        (error (format nil "tried to access ~a, not defined" expr-name)))))

(defun peg-partial (&rest defs) 
  (apply #'concatenate 'string
         (mapcar 
           (lambda (el) 
             (format nil "~a~&" (expr el))) 
           defs)
         ))

#+5am
(5am:def-suite* markdown-spacing-suite :in markdown-grammar-suite)

#+nil
(5am:test newline-test 
  (5am:is (test-match-literal 
            '(:spacechar)
            (funcall
              (generate 
                (parse #'pattern (expr "Spacechar")) 
                ;:symbols-only t
                )
              " "))))

#+5am
(with-grammar-def 
  (expr "Spacechar")
  (space-test
    (:spacechar)
    " "))
#+5am
(with-grammar-def 
  (peg-partial "BlankLine"
               "Sp"
               "Spacechar"
               "Newline"
               )
  (blankline-test
    (:blankline
      (:sp (:spacechar))
      (:newline))
    #?" \n")
  (blankline-empty-test
    (:blankline
      (:sp)
      (:newline))
    #?"\n"))

