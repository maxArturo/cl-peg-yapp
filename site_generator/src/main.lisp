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
;; sample usage
;; (load-config :filename "/Users/max/Developer/hard_lisp/site_generator/site.toml")

;; lets make this a plist
(defun toml-to-plist (file-list-input)
    (for:for ((raw-lines in file-list-input)
          (lines unless (equal "" raw-lines) = raw-lines)
          (splits = (uiop:split-string lines :separator "="))
          (key-values 
            = (destructuring-bind (key-str val-str) splits 
                (list (read-from-string (string-trim " " key-str)) (string-trim "\" " val-str))))
          (ans reducing key-values :by (lambda (curr-val acc) (append curr-val acc))))))
;; (toml-to-plist *site-config*)


