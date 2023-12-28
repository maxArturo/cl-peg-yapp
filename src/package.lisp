(in-package #:cl-user)

(uiop:define-package #:cl-peg-yapp 
                     (:export 
                       #:peg-suite
                       #:parser-suite
                       #:grammar-suite
                       #:generator-suite
                       #:generator-suite
                       #:grammar-definition-suite 
                       #:scanner-suite)
                     (:use 
                       #:cl)) 

(uiop:define-package #:cl-peg-yapp/peg-parser
                     (:export 
                       #:test-input
                       #:test-match-literal
                       #:any-char
                       #:char-literal
                       #:char-range
                       #:compact-match
                       #:compose
                       #:copy-match
                       #:defexpr
                       #:defexpr-class
                       #:defpattern
                       #:display-readable-chars
                       #:empty-match
                       #:make-match
                       #:match-children
                       #:match-end
                       #:match-kind
                       #:match-p
                       #:match-start
                       #:match-str
                       #:negative-lookahead
                       #:new-match
                       #:one-or-more
                       #:opt-expr
                       #:or-expr
                       #:parse
                       #:positive-lookahead
                       #:pprint-peg-match
                       #:string-expr
                       #:times
                       #:with-caching
                       #:zero-or-more
                       #:min-max-times
                       #:test-full-match)
                     (:use 
                       #:cl 
                       #:cl-peg-yapp)
                     (:import-from 
                       #:cl-peg-yapp
                       #:parser-suite)
                     (:import-from #:alexandria
                      #:ensure-list #:flatten))

(uiop:define-package #:cl-peg-yapp/peg-grammar 
                     (:export
                       #:end-line
                       #:optional
                       #:spacing
                       #:char-range-literal
                       #:simple
                       #:quant
                       #:upper-case
                       #:comment-line
                       #:uphex
                       #:sequence-expr
                       #:definition
                       #:lower-case
                       #:pos-look
                       #:grammar
                       #:unicode-class
                       #:check-id
                       #:neg-look
                       #:amount
                       #:rule
                       #:alpha-class
                       #:range-expr
                       #:range-char-option
                       #:comment-endline
                       #:digit
                       #:min-one
                       #:min-max-amount
                       #:string-class
                       #:unicode
                       #:string-literal
                       #:primary
                       #:expression
                       #:grammar
                       #:pattern
                       #:alphanum-class
                       #:numeric-class
                       #:wildcard-class
                       #:unipoint-class
                       #:min-zero)
                     (:use #:cl #:cl-peg-yapp/peg-parser)
                     (:import-from #:cl-peg-yapp #:grammar-suite)
                     )

(uiop:define-package #:cl-peg-yapp/peg-generator
                    (:import-from #:alexandria
                      #:flatten)
                     (:import-from #:cl-peg-yapp #:generator-suite)
                     (:use #:cl 
                      #:cl-peg-yapp/peg-parser 
                      #:cl-peg-yapp/peg-grammar)
                     (:export #:generate))

(uiop:define-package #:cl-peg-yapp/peg-scanner
                     (:use 
                       #:cl 
                       #:cl-peg-yapp/peg-parser 
                       #:cl-peg-yapp/peg-grammar 
                       #:cl-peg-yapp/peg-generator)
                     (:import-from #:cl-peg-yapp #:scanner-suite)
                     (:export #:parse-grammar))

(uiop:define-package #:cl-peg-yapp/peg-definition-tests
                     (:use 
                       #:cl 
                       #:cl-peg-yapp 
                       #:cl-peg-yapp/peg-parser 
                       #:cl-peg-yapp/peg-grammar 
                       #:cl-peg-yapp/peg-generator
                       #:cl-peg-yapp/peg-scanner))
