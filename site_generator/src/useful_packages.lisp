(load "~/quicklisp/setup.lisp")
(ql:quickload "trivia")
(ql:quickload "fiveam")
(ql:quickload "cl-ppcre") ;;regexes
(ql:quickload "for") 
(ql:quickload "alexandria") 
; (load "~/Developer/hard_lisp/site_generator/sitegen.asd")
(load "~/Developer/hard_lisp/peg/cl-peg.asd")
(asdf:make "cl-peg")
; (asdf:test-system "cl-peg")
(5am:run! 'peg:peg-suite)
; (5am:run! 'sitegen:sitegen-tests)

