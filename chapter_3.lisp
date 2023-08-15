;; a plist is a property list, with symbols and alternating data.

;; (defvar *some-local-var*  (list :a 1 :b 2 :c 3))

;; you can get the data from here, with `getf`. A poor man's hash table of sorts.

;; (getf *some-local-var* :c)

;; let's start making some data.

(defun make-post (title content points date)
    (list 
         :title title 
         :content content 
         :points points
         :date date))

(make-post "sample title" "sample content" 18 "2023-08-15")

;; let's make a db var that holds everything
(defvar *db* nil)

;; and let's add a function that handles the adding of things
(defun add-post (post) (push post *db*))

;; now we can add posts to our db
(add-post (make-post "some title" "some content" 18 "2023-08-15")) 
(add-post (make-post "new title" "new content" 42 "2023-08-15")) 
(add-post (make-post "second title" "second content" 18 "2023-08-15")) 
(add-post (make-post "third title" "third content" 38 "2023-08-15")) 


;; let's make the output look nice

(defun dump-db () 
    (dolist (cd *db*) (format t "~{~a:~10t~a~%~}~%" cd)))
(dump-db)

;; t is shorthand for the stream *standard-output*.
;; The ~t directive is for tabulating. The ~10t tells FORMAT to 
;; emit enough spaces to move to the tenth column before processing 
;; the next ~a. A ~t doesn't consume any arguments. 

;; CL-USER> (format t "~a:~10t~a" :artist "Dixie Chicks")
;; ARTIST:   Dixie Chicks
;; NIL
;; 
;; Now things get slightly more complicated. When FORMAT sees ~{
;; the next argument to be consumed must be a list. FORMAT loops 
;; over that list, processing the directives between the ~{ and 
;; ~}, consuming as many elements of the list as needed each 
;; time through the list. In dump-db, the FORMAT loop will consume 
;; one keyword and one value from the list each time through the 
;; loop. The ~% directive doesn't consume any arguments but tells 
;; FORMAT to emit a newline. Then after the ~} ends the loop, the 
;; last ~% tells FORMAT to emit one more newline to put a blank 
;; line between each CD.
;; 
;; Technically, you could have also used FORMAT to loop over the database itself, turning our dump-db function into a one-liner.
;; 
;; (defun dump-db ()
;;     (format t "~{~{~a:~10t~a~%~}~%~}" *db*))
;; 
;; That's either very cool or very scary depending on your point of view. 

;; oh boy. now lets do user interaction.

;; note that The variable *query-io* is a global variable 
;; (which you can tell because of the * naming convention for global variables)
;; that contains the input stream connected to the terminal.

(defun prompt-read (prompt) 
  (format *query-io* "~a: " prompt)
  (force-output *query-io*) ; needed in some implementations to make sure we
                            ; don't wait for newline before printing
  (read-line *query-io*))

;; now we can build and store an entire record.
(defun prompt-post ()
  (make-post 
    (prompt-read "title")
    (prompt-read "content")
    ;; except the madeup field points is an int - and we read in strings.    
    ;; so we need to coerce that with: 
    (or (parse-integer (prompt-read "points") :junk-allowed t) 0) ;or returns NIL 
                                                                  ;if given junk  
    (prompt-read "date")))

;; finally we can wrap it all in a loop-based multirecord until the user
;; wants to stop.

(defun prompt-multi-posts ()
  (loop (add-post (prompt-post))
        (if (not (y-or-n-p "Another? [y/n]: ")) 
            (return))))

;; Let's save this DB.
(defun save-db (&key (filename "posts.db")) ; ensures provided by keys
                                            ; also "posts.db" is default
  (with-open-file (out filename 
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))

;; we can also load it back!

(defun load-db (&rest filename) 
  "loads the db of posts. Default db location is `posts.db`"
  (with-open-file (in (or (car filename) "posts.db"))
    (with-standard-io-syntax (setf *db* (read in)))))

;; now we're building a querying language for this (for the sake
;; of practice. 

;; let's get a function that allows us to search for a particular field.
;; we need to use `getf` and the field we're looking for.
;; for example we want to see if a particular post has this title:

(equal "magical title" (getf *some-local-var* :a))
;; now we can treat the whole db as a list (inefficient but this is practice
;; ) and search for all the records that have the thing

;; (remove-if-not 
;;   (lambda (post) (equal (getf post :title) "hey")) 
;;   *db*)

;; now we can take all that as a higher order function
(defun select(selector) (remove-if-not selector *db*))

;; and now we write a post tile-specific selector: 
(defun select-by-title (title) 
  (select(lambda (post) (equal title (getf post :title)))))

;; but now we want to ggenerate a bunch of these functions, each
;; with their own variation. With the only caveat that each look for
;; a particular field, and have a particular argument

;; this `where` works as in SQL, with implicit *and*
(defun where (&key title content (points nil points-p) date)
  (lambda (post) 
    (and 
      (if title (equal title (getf post :title)) t)
      (if content (equal content (getf post :content)) t)
      (if points-p (equal points (getf post :points)) t)
      (if date (equal date (getf post :date)) t))))

;; and yes, why not. Let's add update.
(defun update (selector-fn &key title content (points nil points-p) date) 
  (setf *db*
        (mapcar 
          (lambda (row) 
            (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if content (setf (getf row :content) content))
               (if points-p (setf (getf row :points) points))
               (if date (setf (getf row :date) date)))
          row) 
         *db*)))
;; ooooo boy. macros


;; OK so let's remove repetition in *where*. Ideally we'd want to
;; make sure we only compare for the clauses and fields we're interested
;; in. 

;; To do so let's make a function that "writes" what we want
(defun make-comparison-expr (field value)
  `(equal ,value (getf post ,field)))

;; wow that's powerful
;; ok now let's put it all together
(defun make-comparison-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

;; last thing remaining is to wrap everything in AND and you're set!
(defmacro where (&rest clauses) 
  `(lambda (post) (and ,@(make-comparison-list clauses))))

;; you can see what would come out with macroexpand-1

;; CL-USER> (macroexpand-1 '(where :title "yoo" :date "some date"))
;; (LAMBDA (POST)
;;   (AND (EQUAL "yoo" (GETF POST :TITLE)) (EQUAL "some date" (GETF POST :DATE))))



