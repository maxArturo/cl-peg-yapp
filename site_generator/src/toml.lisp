(in-package #:sitegen)         

;; This isn't even a halfhearted attempt at the full spec,
;; just the bare need for kicks

#+5am
(5am:in-suite toml-suite)

(defmacro string-assertion (assertion-name doc-string assertion)
  "macro for general-purpose string assertions. 
   Clauses must be a lambda that takes a `line' var and returns
   T or NIL."
  (declare (string doc-string))
  `(defun ,assertion-name (line) ,doc-string 
     (declare (string line))
     (,assertion line)))

#+5am
(5am:test some-test
 (5am:is 't))


(string-assertion is-empty 
  "returns T if string is empty, NIL otherwise"
  (lambda (x) (equal "" x)))

(string-assertion is-comment
  "returns T if it is considered a comment, NIL otherwise"
  (lambda (x) 
    (equal #\# (uiop:first-char (string-left-trim " " x)))))

(defun trim (str)
  "simple empty space trim"
  (declare (string str))
  (string-trim " " str))

(defun get-valid-keyval (str)
  "returns a plist for a valid simple TOML key-value string of the form
    `landing_page = \"yo_chancho.md\"`. returns NIL otherwise"
    (match (uiop:split-string str :separator "=") 
      ((list key val)
       (cond ((and 
         (not (is-empty key))
         (not (is-empty val))) 
              (list (read-from-string (trim key))  (trim val)))
             ('t nil)))))


(defun toml-to-plist (file-list-input)
    (for:for ((raw-lines in file-list-input)
          (lines unless (equal "" raw-lines) = raw-lines)
          (splits = (uiop:split-string lines :separator "="))
          (key-values 
            = (destructuring-bind (key-str val-str) splits 
                (list (read-from-string (string-trim " " key-str)) (string-trim "\" " val-str))))
          (ans reducing key-values :by (lambda (curr-val acc) (append curr-val acc))))))

