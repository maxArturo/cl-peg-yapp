#!/bin/sh

rlwrap sbcl --non-interactive \
    --eval '(require :asdf)' \
    --eval '(load "~/quicklisp/setup.lisp")' \
    --load run-tests.lisp
