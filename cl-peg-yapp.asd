(asdf:defsystem cl-peg-yapp
  :name "cl-peg-yapp"
  :version "0.1"
  :serial t
  :author "Max Alcala <maxarturo@gmail.com>"
  :maintainer "Max Alcala <maxarturo@gmail.com>"
  :license "MIT"
  :description "homebrew parsing expression grammar utility"
  :encoding :utf-8
  :depends-on ("trivia" "for" "cl-ppcre" "alexandria" "cl-interpol")
  :pathname "src/"
  :components
  ((:file "package")
   (:file "test-suites")
   (:module "parser"
            :components
            ((:file "tree")
             (:file "entities")
             (:file "patterns")))
   (:module "peg-grammar"
            :components
            ((:file "comment")
             (:file "quant")
             (:file "literal")
             (:file "sequence")
             (:file "grammar")))
   (:file "scanner")))
