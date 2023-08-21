(asdf:defsystem cl-peg
  :name "cl-peg"
  :version "0.1"
  :author "Max Alcala <maxarturo@gmail.com>"
  :maintainer "Max Alcala <maxarturo@gmail.com>"
  :license "MIT"
  :description "homebrew parsing expression grammar utility"
  :encoding :utf-8
  :depends-on ("trivia" "for" "cl-ppcre" "alexandria")
  :pathname "src/"
  :perform (asdf:test-op (o c) (uiop:symbol-call :fiveam '#:run! 'cl-peg:peg-tests))
  :components ((:file "package")
               (:file "testsuite" :depends-on ("package"))
               (:module "grammar" 
                    :components ((:file "grammar")))))

