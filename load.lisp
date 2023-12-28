; used to quickly make sure everything is loaded and tested
; before you start developing

(load "~/quicklisp/setup.lisp")
(ql:quickload "fiveam")
(ql:quickload "cl-peg-yapp")
(interpol:enable-interpol-syntax)
(5am:run! 'cl-peg-yapp:peg-suite)

;; build and test everything
#+nil
(progn 
  (ql:quickload "trivia")
  (ql:quickload "cl-ppcre")
  (ql:quickload "for")
  (ql:quickload "cl-interpol")
  (ql:quickload "alexandria")
  (ql:quickload "fiveam")
  (load "~/Developer/cl-peg-yapp/cl-peg-yapp.asd")
  (interpol:enable-interpol-syntax)
  (asdf:make "cl-peg-yapp")
  (asdf:load-system "cl-peg-yapp" :force t)
  (5am:run! 'cl-peg-yapp:peg-suite))

#+nil
(5am:run! 'cl-peg-yapp:grammar-suite)
#+nil
(5am:run! 'cl-peg-yapp:parser-suite)
#+nil
(5am:run! 'cl-peg-yapp:scanner-suite)
#+nil
(5am:run! 'cl-peg-yapp:generator-suite)
#+nil
(5am:run! 'cl-peg-yapp:grammar-definition-suite)
#+nil
(5am:run! 'cl-peg-yapp:peg-suite)
#+nil
(list-all-packages)

