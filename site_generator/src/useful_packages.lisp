(load "~/quicklisp/setup.lisp")
(ql:quickload "trivia") for pattern matching
(ql:quickload "cl-ppcre") ;;regexes
(ql:quickload "for") structured looping
(load "~/Developer/hard_lisp/site_generator/sitegen.asd")
(load "~/Developer/hard_lisp/peg/cl-peg.asd")
(asdf:make "cl-peg")
(asdf:test-system "cl-peg")
(5am:run! 'sitegen:sitegen-tests)

