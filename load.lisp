; used to quickly make sure everything is loaded and tested
; before you start developing


(load "~/quicklisp/setup.lisp")
(ql:quickload "trivia")
(ql:quickload "fiveam")
(ql:quickload "cl-ppcre") 
(ql:quickload "for") 
(ql:quickload "cl-interpol") 
(ql:quickload "alexandria") 
(load "~/Developer/cl-peg-yapp/cl-peg-yapp.asd")
(asdf:make "cl-peg-yapp")
(asdf:load-system "cl-peg-yapp" :force t)
(interpol:enable-interpol-syntax)
(5am:run! 'peg:peg-suite)


