(in-package #:sitegen)

;; full test suite definition here
;; most tests will live next to their contextual unit functions

#+5am
(5am:def-suite sitegen-tests :description "sitegen test suite")

#+5am
(5am:def-suite toml-suite
  :description "Test toml functionality"
  :in sitegen-tests)

