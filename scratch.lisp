;(describe 'standard-char)
;,delete-system-fasls RET system name to remove everything 
(load "~/quicklisp/setup.lisp")
(ql:quickload "trivia")
(ql:quickload "fiveam")
(ql:quickload "cl-ppcre") 
(ql:quickload "for") 
(ql:quickload "cl-interpol") 
(ql:quickload "alexandria") 
(load "~/Developer/cl-peg/cl-peg.asd")
(asdf:make "cl-peg")
(asdf:load-system "cl-peg" :force t)
(interpol:enable-interpol-syntax)
(5am:run! 'peg:parser-suite)


