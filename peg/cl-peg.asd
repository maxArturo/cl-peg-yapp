(asdf:defsystem cl-peg
  :name "cl-peg"
  :version "0.1"
  :serial t
  :author "Max Alcala <maxarturo@gmail.com>"
  :maintainer "Max Alcala <maxarturo@gmail.com>"
  :license "MIT"
  :description "homebrew parsing expression grammar utility"
  :encoding :utf-8
  :depends-on ("trivia" "for" "cl-ppcre" "alexandria")
  :pathname "src/"
  :components (
               (:file "package")
               (:file "test-suites")
               (:file "grammar")
               (:file "grammar-test")))

;; (defsystem "cl-peg-tests"
;;   :serial t
;;   :depends-on ("cl-peg" "fiveam")
;;   :components ((:file "test-suites")
;;                (:file "grammar-test"))
;;   :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run! :peg-suite)))
