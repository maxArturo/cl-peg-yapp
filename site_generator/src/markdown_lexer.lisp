(in-package #:sitegen)         

;; read from a file

(defvar *file-contents*)

(defun load-file (filename) 
  "loads the file contents into the *file-contents* var"
  (declare (string filename))
  (setf *file-contents* (uiop:read-file-string filename)))

;; sample usage
(load-file "/Users/max/Developer/hard_lisp/site_generator/test.md")

(ppcre:scan (getf token-matcher :atx-header) *file-contents*) 
(length *file-contents*)
;; let's start the easiest way. just have a couple of tokens
;; how hard can it be :)
(defparameter token-matcher 
  (list 
    :atx-header (ppcre:create-scanner "^(#{1,6})(.*)\n" :multi-line-mode t)
    :paragraph (ppcre:create-scanner "^.+$" :multi-line-mode t)))

(defun is-header (input) 
  (let ((pattern (ppcre:create-scanner "^#{1,6}" :multi-line-mode t))))
  ()
  )
(ppcre:scan "^(?:#{1,6})(.*)$" "## durrf one tho three" :multi-line-mode t)


(defun tokenize (input) 
  (accumulate-tokens input '()))

(defun accumulate-tokens (rem-input &key (token-list)))

