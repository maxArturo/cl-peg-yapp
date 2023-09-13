;(describe 'standard-char)
;,delete-system-fasls RET system name to remove everything 

(coerce "hey you" 'list)
; base functionality
(funcall #'peg-parser::char-terminal 
  (coerce "hey you" 'list))

(funcall (funcall 'peg-parser::literal-char-terminal #\f)
  (coerce "figaro" 'list)) 

; negate
(funcall 
  (funcall 'peg-parser::negate  'peg-parser::char-terminal) 
  (coerce "figaro" 'list)) 

(funcall 
  (funcall 
    'peg-parser::negate  
    (funcall 'peg-parser::literal-char-terminal #\f))
  (coerce "hfigaro" 'list)) 

; compose
(funcall 
  (funcall #'peg-parser::compose
    (funcall #'peg-parser::literal-char-terminal #\f) 
    (funcall #'peg-parser::literal-char-terminal #\i) 
    (funcall #'peg-parser::literal-char-terminal #\g) 
    (funcall #'peg-parser::literal-char-terminal #\a))
  (coerce "figar" 'list))

