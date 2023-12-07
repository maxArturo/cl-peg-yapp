(in-package #:cl-user)

; needed to enable string interpolation
(interpol:enable-interpol-syntax)

(uiop:define-package 
  #:cl-peg-yapp
  (:use #:cl)
  (:export #:peg-suite
   #:parser-suite
   #:grammar-suite
   #:scanner-suite)) 

(uiop:define-package 
  #:cl-peg-yapp/peg-parser
  (:use #:cl #:cl-peg-yapp)
  (:import-from #:cl-peg-yapp
   #:parser-suite)
  (:import-from #:alexandria
   #:ensure-list
   #:flatten)
  (:export
    #:match
    #:empty-match
    #:any-char
    #:char-literal
    #:char-range
    #:positive-lookahead
    #:negative-lookahead
    #:compose
    #:times
    #:zero-or-more
    #:one-or-more
    #:opt-expr
    #:or-expr
    #:string-expr
    #:defexpr
    #:defpattern
    #:parse
    ))

(uiop:define-package 
  #:cl-peg-yapp/peg-grammar
  (:use #:cl #:cl-peg-yapp/peg-parser)
  (:import-from #:cl-peg-yapp #:grammar-suite)
  (:export 
    #:spec))

(uiop:define-package 
  #:cl-peg-yapp/peg-scanner
  (:import-from #:cl-peg-yapp #:scanner-suite)
  (:use #:cl #:cl-peg-yapp/peg-parser #:cl-peg-yapp/peg-grammar))

