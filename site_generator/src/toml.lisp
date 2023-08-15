(in-package #:sitegen)         

;; This isn't even a halfhearted attempt at the full spec,
;; just the bare need for kicks

#+5am
(5am:in-suite toml-suite)

(defun is-empty (str)
  "returns T if string is empty, NIL otherwise"
  (declare (string str))
  (equal "" str))

#+5am
(5am:test is-empty-test
    (5am:is (eq NIL (is-empty "funky")))
    (5am:is (eq T (is-empty ""))))

(defun is-comment (str)
  "returns T if str is considered a comment, NIL otherwise"
  (equal #\# (uiop:first-char (string-left-trim " " str))))

#+5am
(5am:test is-comment-test
  (for:for 
    (((&key expected input) in 
        '((:expected T :input "# ")
          (:expected NIL :input " hello ")
          (:expected T :input " # hell "))))
    (5am:is (equal expected (is-comment input)))))

(defun trim (str &key (extra-chars ""))
  "simple empty space trim"
  (declare (string str extra-chars))
  (string-trim (concatenate 'string " " extra-chars) str))

#+5am
(5am:test trim-test
  (for:for 
    (((&key expected input extra-chars) in 
        '((:expected "" :input " ")
          (:expected "hello" :input " hello         ")
          (:expected "chancho.md" :input "\"chancho.md\"" :extra-chars "\"")
          (:expected "hello         ." :input " hello         . " :extra-chars "\""))))
    (5am:is (equal expected (trim input :extra-chars (or extra-chars ""))))))

(defun get-valid-keyval (str)
  "returns a plist for a valid simple TOML key-value string of the form
    `landing_page = \"yo_chancho.md\"`. returns NIL otherwise"
    (match (uiop:split-string str :separator "=") 
      ((list key val)
       (cond ((and 
         (not (is-empty key))
         (not (is-empty val))) 
              (list (read-from-string (concatenate 'string ":" (trim key)))  
                    (trim val :extra-chars "\"")))
             ('t NIL)))))

#+5am
(5am:test get-valid-keyval-test
  (for:for 
    (((&key expected input) in 
        '((:expected NIL :input " ")
          (:expected (:hello "chancho.md") :input " hello = \"chancho.md\"")
          (:expected NIL :input " hello  \"chancho.md\"")
          (:expected (:location "/usr/share/bin.bin") :input "location = \"/usr/share/bin.bin\"")
          (:expected (:bar "foo.baz") :input " bar = \" foo.baz\""))))
    (cond ((eq NIL expected) (5am:is (eq NIL (get-valid-keyval input))))
          (T (5am:is (equal (get-valid-keyval input) expected))))))

(defun toml-to-plist (file-list-input)
  (for:for 
    ((raw-lines in file-list-input)
     (lines unless (is-empty raw-lines) = raw-lines)
     (splits unless (is-comment lines) = (get-valid-keyval lines))
     (ans reducing splits :by (lambda (curr-val acc) (append curr-val acc))))))

