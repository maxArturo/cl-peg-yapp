(asdf:defsystem sitegen
  :name "sitegen"
  :version "0.1"
  :author "Max Alcala <maxarturo@gmail.com>"
  :maintainer "Max Alcala <maxarturo@gmail.com>"
  :license "MIT"
  :description "simple site generator"
  :encoding :utf-8
  :depends-on (trivia for)
  :components
  ((:module src
    :pathname "src"
    :components ((:file "package")
		 (:file "testsuite" :depends-on ("package"))
		 (:file "main" :depends-on ("package" "testsuite"))
		 (:file "toml" :depends-on ("package" "testsuite"))))))

