(in-package #:sitegen)

#+5am
(5am:def-suite sitegen-tests :description "sitegen test suite")

#+5am
(5am:def-suite toml-suite
  :description "Test toml functionality"
  :in sitegen-tests)

