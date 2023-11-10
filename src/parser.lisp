;; base parser
;; heavily based on the original paper and
;; https://github.com/PhilippeSigaud/Pegged/wiki/PEG-Basics

(in-package #:peg-parser)

#+5am
(5am:in-suite parser-suite)

(defvar *input-buffer*)

(defun load-config (&key (filename "input.peg")) 
  "loads the input grammar file (*.peg)"
  (setf *input-buffer* (uiop:read-file-string filename)))

;; sample usage
(load-config :filename "/Users/max/Developer/cl-peg-yapp/test_grammar.peg")

;; go from lowest to highest precedence
(defun load-grammar (grammar-str)
  "loads a grammar from a string representation"
  (let ((lines (uiop:split-string grammar-str :separator 
                                  '(#\return #\newline))))
   lines 
    ))
(load-grammar *input-buffer*) 



; an expression applies its rules greedily. It will:
; - view the next char, and either: 
;   - fail on its condition
;   - advance to next char while
;     - its condition is true or
;     - EOF
; the parsed output is a chain of these expressions.

(defparameter *terminal-expressions*
  (list :literal :eps :dot :range))


