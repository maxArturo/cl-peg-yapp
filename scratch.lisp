;(describe 'standard-char)
;,delete-system-fasls RET system name to remove everything 

(coerce "hey you" 'list)
; base functionality
(funcall (funcall #'peg-parser::char-terminal) 
  (coerce "hey you" 'list))

(funcall (funcall #'peg-parser::char-terminal) 
  (coerce "" 'list))

(funcall (funcall 'peg-parser::literal-char-terminal #\f)
  (coerce "figaro" 'list)) 

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
(line-end (coerce "jigaro" 'list))
(line-end (list #\CR))

; parse peg comment
(comment-line (coerce "#jigaro" 'list))
(comment-line (coerce "jigaro" 'list))
(comment-line (coerce "   ### jigaro" 'list))
(comment-line (list #\CR))
