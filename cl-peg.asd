(asdf:defsystem cl-peg
  :name "cl-peg"
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
    (:file "parser")
    (:module "peg-grammar"
      :components 
      ((:file "structs")
       (:file "base")
       (:file "literal")
       (:file "quant")
       (:file "expressions")))))

