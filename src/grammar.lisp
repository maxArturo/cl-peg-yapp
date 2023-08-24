(in-package #:peg-parser)

#+5am
(5am:in-suite parser-suite)

(defvar *input-buffer*)

(defun load-config (&key (filename "input.peg")) 
  "loads the input grammar file (*.peg)"
  (setf *input-buffer* (uiop:read-file-lines filename)))

;; sample usage
;; (load-config :filename "/Users/max/Developer/hard_lisp/site_generator/site.toml")


(defmacro char-kind (name &rest forms)
  "generates kinds of character function
   tests"
  `(defun ,name (in)
     (and (characterp in)
          ,@(or forms '(T)) 
          )))

;; char definitions
;; note that these definitions are much wider and more permissive
;; than those in the spec

(char-kind chr)

(char-kind unipoint (<= 32 (char-code in) 126))

(char-kind widepoint (<= 255 (char-code in)))

(char-kind space-chr (= 32 (char-code in)))

(char-kind endline (find in '(#\cr #\lf)))

(let ((line-end-str (coerce (list #\cr #\lf) 'string))) 
  (defun line-end (in) 
    (or (eql in line-end-str) (endline in))))

(char-kind lower (<= 97 (char-code in) 122))

(char-kind upper (<= 65 (char-code in) 90))

(defun is-comment (grammar-line)
  (and 
    (not (uiop:emptyp grammar-line))
    (char= #\# 
         (uiop:first-char 
           (string-left-trim " " grammar-line)))))
#+5am
(5am:test is-comment-test
  (for:for 
    (((&key expected input) in 
        '((:expected T :input "# ")
          (:expected NIL :input " hello ")
          (:expected T :input " # hell "))))
    (5am:is (equal expected (is-comment input)))))
 
; ;; productions
(defun find-line-end (input) 
  (or (find #\cr input :test #'equal)
      (find #\lf input :test #'equal)
      (search (coerce '(#\cr #\lf) 'string) input)))

; what is a line
; Line <- (!EndLine Char)? EndLine 

(defun char-production (input) 
  (declare (list input))
  (let 
    ((val (car input))) 
    (and (characterp val)
         (cons (make-expression :kind :char :value (list val)) (cdr input)))))
; (char-production (coerce "" 'list))

(defun endline-production (input)
  "returns both a struct and the remaining input to be consumed"
  (trivia:match input 
    ((trivia:guard (list* curr next _)
            (and (eql curr #\CR) (eql next #\LF)))
     (print "first") 
     (list (make-expression 
                   :kind :endline 
                   :value (list curr next)) (cddr input)))
    ((trivia:guard (list* curr _)
            (or (eql curr #\CR) (eql curr #\LF)))

     (print "second") 
     (list (make-expression 
                   :kind :endline 
                   :value (list curr)) (cdr input)))))
 
(endline-production '(#\LF #\a #\y #\o)) 


#+5am
(5am:test endline-production-test
  (for:for 
    (((&key expected input) in 
        (list
          (list 
            :expected (make-expression :kind :endline :value '(#\CR))  
            :input '( #\CR #\a #\y #\o))
          (list 
            :expected (make-expression :kind :endline :value '(#\CR #\LF))  
            :input '(#\CR #\LF #\a #\y #\o))
          (list 
            :expected (make-expression :kind :endline :value '(#\LF))
            :input '( #\LF #\a #\y #\o)))))
    (trivia:match expected
      ((expression kind value)
         (print "expecting struct")
         (print input)
       (let ((res (endline-production input)))
         (print "res")
         (print res)
                (5am:is (equal (expression-kind (car res)) kind))
                (5am:is (equal (expression-value (car res)) value))))
      ((T) 
         (print "expecting nothing")
         (print expected)

         (print "input")
         (print input)
         ;(print (endline-production input))
       (5am:is (null (endline-production input)))))))


; what is a comment?
; in PEG terms: 
; Comment <- SP* '#' (word)
(defun comment-production (input) 
  "attempts to match a comment production rule for
   the PEG definition grammar. Reads a comment starting
   with a `#` (trimmed) till the next end of line char(s)."
  (if (is-comment input) 
      (let ((pos (find-line-end input))) 
           (make-expression :kind :comment :value (subseq input 0 pos)))
      NIL))
#+5am
(5am:test comment-production-test
  (for:for 
    (((&key expected input) in 
        '((:expected T :input "# ")
          (:expected NIL :input " hello ")
          (:expected T :input " # hell "))))
    (5am:is (equal expected (is-comment input)))))

