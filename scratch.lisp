(defun show-all-fboundp-symbols-of-package
    (package-name
     &optional (stream t))
  (let ((pack (find-package package-name)))
    (do-all-symbols (sym pack)
      (when (eql (symbol-package sym) pack)
        (when (fboundp sym)
          (format stream ":~A~%" (symbol-name sym)))))))
#+nil
(show-all-fboundp-symbols-of-package 'cl-peg-yapp)
(show-all-fboundp-symbols-of-package 'cl-peg-yapp/peg-parser)
(show-all-fboundp-symbols-of-package 'cl-peg-yapp/peg-grammar)

