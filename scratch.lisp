; to pull up docs on the REPL:
; (describe 'standard-char)

; to remove everything loaded:
; ,delete-system-fasls RET system name 

(nth 0 (coerce "hey youn" 'list))


(nthcdr 0 (coerce "hey youn" 'list))

; base functionality
(funcall (funcall #'peg-parser::char-terminal)
  (coerce "hey you" 'list))

(funcall (funcall #'peg-parser::char-terminal)
  (coerce "" 'list))

(funcall (funcall 'peg-parser::literal-char-terminal #\f)
  (coerce "figaro" 'list))

; char range 
;fails
(funcall (char-range-terminal #\b #\e)
  (coerce "figaro" 'list))

;succeeds
(funcall (char-range-terminal #\b #\e)
  (coerce "bigaro" 'list))

; negate
(funcall
    (funcall #'peg-parser::negate (funcall 'peg-parser::literal-char-terminal #\i))
  (coerce "figaro" 'list))

(funcall
    (funcall
        'peg-parser::negate
      (funcall 'peg-parser::literal-char-terminal #\f))
  (coerce "figaro" 'list))

; compose
(funcall
    (funcall #'peg-parser::compose
      (funcall #'peg-parser::literal-char-terminal #\f)
      (funcall #'peg-parser::literal-char-terminal #\i)
      (funcall #'peg-parser::literal-char-terminal #\g)
      (funcall #'peg-parser::literal-char-terminal #\a))
  (coerce "figar" 'list))

(funcall
    (funcall #'peg-parser::compose
      (funcall #'peg-parser::literal-char-terminal #\i)
      (funcall #'peg-parser::literal-char-terminal #\g)
      (funcall #'peg-parser::literal-char-terminal #\a))
  (coerce "figar" 'list))

; zero or more
(funcall
    (funcall #'peg-parser::zero-or-more
      (funcall #'peg-parser::literal-char-terminal #\f))
  (coerce "booyah" 'list))

(funcall
    (funcall #'peg-parser::zero-or-more
      (funcall #'peg-parser::char-terminal))
  (coerce "hello" 'list))

; one or more
(funcall
    (funcall #'peg-parser::one-or-more
      (funcall #'peg-parser::literal-char-terminal #\f))
  (coerce "igaro" 'list))

(funcall
    (funcall #'peg-parser::one-or-more
      (funcall #'peg-parser::literal-char-terminal #\f))
  (coerce "figaro" 'list))

; optional
(funcall
    (funcall #'peg-parser::optional
      (funcall #'peg-parser::literal-char-terminal #\f))
  (coerce "jigaro" 'list))

(funcall
    (funcall #'peg-parser::optional
      (funcall #'peg-parser::literal-char-terminal #\f))
  (coerce "figaro" 'list))

; parse line end
(funcall (line-end) (coerce "jigaro" 'list))
(funcall (line-end) (list #\CR))
(funcall (line-end) (coerce "#jigaro" 'list))

; parse peg comment
(funcall (comment-line) (coerce "jigaro" 'list))
(funcall (comment-line) (coerce "   ### jigaro" 'list))
(funcall (comment-line) (list #\CR))


(in-package #:peg-grammar)
(funcall 
  (zero-or-more #'comment-endline)
  (coerce
"# this is a test spec
# it's weird
# but it's valid
Word <- Letter+ # with comments too! 
Letter <- [A-Za-z] " 'list) 0)

; currently we're trying to make comapct trees work
; but the issue is some results come back as atoms (not lists)
; ex: 

(in-package #:peg-grammar)
(funcall #'upper-case (coerce "First" 'list) 0)
  ; #S(MATCH
  ;    :STR (#\F #\i #\r #\s #\t)
  ;    :START 0
  ;    :END 1
  ;    :CHILDREN #S(MATCH
  ;                 :STR (#\F #\i #\r #\s #\t)
  ;                 :START 0
  ;                 :END 1
  ;                 :CHILDREN NIL
  ;                 :KIND NIL)
  ;    :KIND :UPPER-CASE)

; I just had to re-eval everything (as the macro defuns things). and now it works: 

(funcall #'upper-case (coerce "First" 'list) 0)
  ; #S(MATCH
  ;    :STR (#\F #\i #\r #\s #\t)
  ;    :START 0
  ;    :END 1
  ;    :CHILDREN (#S(MATCH
  ;                  :STR (#\F #\i #\r #\s #\t)
  ;                  :START 0
  ;                  :END 1
  ;                  :CHILDREN NIL
  ;                  :KIND NIL))
  ;    :KIND :UPPER-CASE)

; now I can focus on the actual compact match:

(funcall #'upper-case (coerce "First" 'list) 0)
  ; #S(MATCH
  ;    :STR (#\F #\i #\r #\s #\t)
  ;    :START 0
  ;    :END 1
  ;    :CHILDREN NIL
  ;    :KIND :UPPER-CASE)

; nice this looks good. Let's look at bigger functions...
(funcall #'end-line (list #\CR #\LF) 0)

; ok this one looks good. How about this
(funcall #'comment-endline (list #\# #\Newline) 0)

; this is wrong. 
  ; PEG-GRAMMAR> (defparameter *compacted-tree* nil)
  ; *COMPACTED-TREE*
  ; PEG-GRAMMAR> (funcall 'comment-endline (list #\# #\Newline) 0)
  ; #S(MATCH
  ;    :STR (#\# #\Newline)
  ;    :START 0
  ;    :END 2
  ;    :CHILDREN (#S(MATCH
  ;                  :STR (#\# #\Newline)
  ;                  :START 0
  ;                  :END 2
  ;                  :CHILDREN (#S(MATCH
  ;                                :STR (#\# #\Newline)
  ;                                :START 0
  ;                                :END 0
  ;                                :CHILDREN NIL
  ;                                :KIND NIL)
  ;                             #S(MATCH
  ;                                :STR (#\# #\Newline)
  ;                                :START 0
  ;                                :END 1
  ;                                :CHILDREN (#S(MATCH
  ;                                              :STR (#\# #\Newline)
  ;                                              :START 0
  ;                                              :END 1
  ;                                              :CHILDREN (#S(MATCH
  ;                                                            :STR (#\# #\Newline)
  ;                                                            :START 0
  ;                                                            :END 0
  ;                                                            :CHILDREN NIL
  ;                                                            :KIND NIL)
  ;                                                         #S(MATCH
  ;                                                            :STR (#\# #\Newline)
  ;                                                            :START 0
  ;                                                            :END 1
  ;                                                            :CHILDREN NIL
  ;                                                            :KIND NIL)
  ;                                                         #S(MATCH
  ;                                                            :STR (#\# #\Newline)
  ;                                                            :START 1
  ;                                                            :END 1
  ;                                                            :CHILDREN NIL
  ;                                                            :KIND NIL))
  ;                                              :KIND NIL))
  ;                                :KIND :COMMENT-LINE)
  ;                             #S(MATCH
  ;                                :STR (#\# #\Newline)
  ;                                :START 1
  ;                                :END 2
  ;                                :CHILDREN (#S(MATCH
  ;                                              :STR (#\# #\Newline)
  ;                                              :START 1
  ;                                              :END 2
  ;                                              :CHILDREN NIL
  ;                                              :KIND NIL))
  ;                                :KIND :END-LINE))
  ;                  :KIND NIL))
  ;    :KIND :COMMENT-ENDLINE)

; but here with the param enabled: 
  ; PEG-GRAMMAR> (defparameter *compacted-tree* t)
  ; *COMPACTED-TREE*
  ; PEG-GRAMMAR> (funcall 'comment-endline (list #\# #\Newline) 0)
  ; #S(MATCH
  ;    :STR (#\# #\Newline)
  ;    :START 0
  ;    :END 2
  ;    :CHILDREN NIL
  ;    :KIND :COMMENT-ENDLINE)
  ; PEG-GRAMMAR> 

; this is too much. Let's try simplyfing
(funcall #'comment-line  (list #\# #\Newline) 0)
(funcall #'comment-line  (list #\# #\Newline) 0)
