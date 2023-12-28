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

(defun test-input (expr str &optional index)
  "prepares a string for expression parsing"
  (declare (string str))
  (funcall expr (coerce str 'list) (or index 0)))

(defun test-match-literal (curr-tree-desc node)
  (let* ((curr-kind (car curr-tree-desc))
         (curr-val (cdr curr-tree-desc))
         (node-str (match-str node))
         (node-start (match-start node))
         (node-end (match-end node))
         (node-kind (and node (match-kind node)))
         (node-literal 
           (and node
                (coerce (subseq node-str node-start node-end)
                        'string))))
    (and 
      (eql curr-kind node-kind)
      (cond ((stringp (second curr-tree-desc))
             (progn
               (string= (second curr-tree-desc) node-literal)))
            ((listp curr-val) 
             (every 
               #'identity
               (mapcar #'test-match-literal curr-val (match-children node))))
            ((eq t curr-val) t)
            (t nil)))))
#+nil
(let ((the-str 
        (coerce 
        "[here's a link](https://www.wikipedia.org)"    
        'list)))
  (test-match-literal 
    '(:markdown 
       (:inline
         (:hyperlinktext "here's a link")
         (:url "https://www.wikipedia.org")))
    (new-match 
      the-str
      0 42 
      (list (new-match 
              the-str 0 42 
              (list
                (new-match the-str 1 14 nil :hyperlinktext)
                (new-match the-str 16 41 nil :url)) 
              :inline))
      :markdown)))


