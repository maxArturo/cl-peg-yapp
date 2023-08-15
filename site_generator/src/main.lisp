;;; static site generation
;;;

;; lets do several things:
;; get the config from a TOML file
;; let's assume the file is called `site.toml`

(defvar *site-config*)

(defun load-config (&key (filename "site.toml")) 
  "loads the site config. Default config location is `site.toml`"
  "we're not even trying to parse correct TOML, just the bare need"
  (setf *site-config* (uiop:read-file-lines filename)))

;; lets make this a plist
(defun toml-to-plist file-list-input
  "turns the raw string list from toml input to a plist"
  (let (lines) (remove-if (string-equal "" line) file-list-input))
  (mapcar 
    (lambda (line) ()) )
  )
;; sample usage
;; (load-config :filename "/Users/max/Developer/hard_lisp/site_generator/site.toml")
  
;; oh god now I need to split...


