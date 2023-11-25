(in-package #:cl)

;; needed to enable interpol syntax
(interpol:enable-interpol-syntax)

(uiop:define-package #:peg
  (:use #:cl)
  (:export #:peg-suite
           #:parser-suite
           #:grammar-suite
           #:scanner-suite)) 

(uiop:define-package #:peg-parser
  (:use #:cl #:peg)
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
   ))

(uiop:define-package #:peg-grammar
  (:use #:cl #:peg #:peg-parser))

(uiop:define-package #:peg-scanner
  (:use #:cl #:peg #:peg-grammar))
