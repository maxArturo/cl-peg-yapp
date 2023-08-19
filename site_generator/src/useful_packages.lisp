(load "~/quicklisp/setup.lisp")
(ql:quickload "trivia") for pattern matching
(ql:quickload "for") structured looping
(load "~/Developer/hard_lisp/site_generator/sitegen.asd")
(5am:run! 'sitegen:sitegen-tests)

