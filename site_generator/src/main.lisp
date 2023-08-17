(in-package #:sitegen)         
;;; static site generation
;;;

;; lets do several things:
;; get the config from a TOML file
;; let's assume the file is called `site.toml`

(defvar *site-config*)

(defun load-config (&key (filename "site.toml")) 
  "loads the site config. Default config location is `site.toml`"
  (setf *site-config* (uiop:read-file-lines filename)))

;; sample usage
;; (load-config :filename "/Users/max/Developer/hard_lisp/site_generator/site.toml")

;; (toml-to-plist *site-config*)
