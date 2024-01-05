(in-package #:cl-peg-yapp/peg-definition-tests)

(interpol:enable-interpol-syntax)
(defvar *base-char-def* "BaseChar <- (Space / [u0020-u007E])")
(defvar *text-def* 
  #?"\
Text <- (!NewLine BaseChar)+ # TODO can be further refined
")



