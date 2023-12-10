;;;; entrypoint for generating a full parser based
;;;; on a provided PEG spec.

; here's a list of all the entities we can match: 
  ; expression
  ; sequence-expr
  ; rule
  ; check-id
  ; plain
  ; pos-look
  ; neg-look
  ; primary
  ; definition
  ; range-expr
  ; unipoint-class
  ; numeric-class
  ; alpha-class
  ; alphanum-class
  ; string-class
  ; end-line
  ; comment-line
  ; comment-endline
  ; spacing
  ; min-max-amount
  ; amount
  ; optional (char-literal #\?) 
  ; min-zero (char-literal #\*) 
  ; min-one (char-literal #\+) 
  ; quant
  ; grammar

(in-package #:cl-peg-yapp/peg-generator)

(defparameter *peg-node-ancestry* nil)

(defmacro gen-node (node-kind &body body) 
  (let ((func-name 
          (intern (format nil "~a-~a" "GEN" (symbol-name node-kind))))
        (sym (intern (symbol-name node-kind) "KEYWORD")))
    `(defun ,func-name (node)
       (trivia:ematch node
         ((match (kind ,sym)
            (str node-str)
            (start node-start)
            (end node-end))
          (let 
            ((*peg-node-ancestry* 
               (concatenate 'list
                 *peg-node-ancestry*
                 '(,sym)))
             (node-literal 
               (coerce (subseq node-str node-start node-end)
                 'string)))
            ,@body))))))

(gen-node grammar 
  (declare (ignore node-literal))
  (mapcar 
    (lambda (el)
      (if (eql (match-kind el) :definition)
          (gen-definition el)))
    (match-children node)))

(gen-node definition
  (declare (ignore node-literal))
  (let ((children (match-children node)))
    `(defexpr ,(gen-check-id (first children))
              ,(gen-expression (second children)))))

(gen-node check-id 
  (intern (string-upcase node-literal)))

; Expression <- Sequence (Spacing* '/' SP* Sequence)*
(gen-node expression 
  (declare (ignore node-literal))
  (mapcar 
    (lambda (el)
      (if (eql (match-kind el) :sequence-expr)
          (gen-sequence-expr el)))
    (match-children node)))

; Sequence <- Rule (Spacing Rule)*
(gen-node sequence-expr 
  (declare (ignore node-literal))
  `(compose )
  )

(defun generate (file)
  (gen-grammar (parse-grammar file)))

(gen-node simple
  (declare (ignore node-literal))
  (let ((kind (match-kind node))
        (child (first (match-children node))))
    (trivia:ematch kind
      (:unicode (gen-unicode child))
      (:range-expr (gen-range-expr child))
      (:string-literal (gen-string-literal child))
      (:alphanum-class (gen-alphanum child))
      (:alpha-class (gen-alpha child))
      (:numeric-class (gen-numeric child))
      (:string-class (gen-string child))
      (:unipoint-class (gen-unipoint child)))))

(gen-node string-literal 
  `(string-expr ,(remove #\' node-literal)))
#+nil
(gen-string-literal (parse #'string-literal "'boom'"))

(gen-node unicode
  `(char-literal 
     ,(code-char 
        (uiop/common-lisp:read-from-string
          (format nil "#x~a" (remove #\u node-literal))))))
#+nil
(gen-unicode (parse #'unicode #?"u009A"))


#+nil
(generate (pathname #p"grammars/basic.peg"))
