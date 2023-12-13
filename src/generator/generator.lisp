;;;; entrypoint for generating a full parser based
;;;; on a provided PEG spec.

; here's a list of all the entities we can match: 
  ; expression
  ; sequence-expr
  ; rule
  ; check-id
  ; plain
  ; pos-look
  ; neg-look
  ; primary
  ; definition
  ; range-expr
  ; unipoint-class
  ; numeric-class
  ; alpha-class
  ; alphanum-class
  ; string-class
  ; end-line
  ; comment-line
  ; comment-endline
  ; spacing
  ; min-max-amount
  ; amount
  ; optional (char-literal #\?) 
  ; min-zero (char-literal #\*) 
  ; min-one (char-literal #\+) 
  ; quant
  ; grammar

(in-package #:cl-peg-yapp/peg-generator)

#+5am
(5am:def-suite* parser-generation-suite :in generator-suite)

(defparameter *peg-node-ancestry* nil)

(defmacro defnode (node-kind &body body) 
  (let ((func-name 
          (intern (format nil "~a-~a" "GEN" (symbol-name node-kind))))
        (sym (intern (symbol-name node-kind) "KEYWORD")))
    `(defun ,func-name (node)
       (trivia:ematch node
         ((match (kind ,sym))
          (let 
            ((*peg-node-ancestry* 
               (concatenate 'list
                 *peg-node-ancestry*
                 '(,sym))))
            ,@body))))))

(defmacro with-node-literal (&body body) 
  `(let* ((node-start (match-start node))
          (node-end (match-end node))
          (node-str (match-str node))
          (node-literal 
            (coerce (subseq node-str node-start node-end)
                    'string)))
     ,@body))

(defmacro with-child ((&key child-kind) &body body)
 `(let* ((child (first (match-children node)))
         ,(if child-kind `(kind (match-kind child))))
      ,@body))

(defnode grammar 
  (mapcar 
    (lambda (el)
      (if (eql (match-kind el) :definition)
          (gen-definition el)))
    (match-children node)))

(defnode definition
  (let ((children (match-children node)))
    `(defexpr ,(gen-check-id (first children))
              ,(gen-expression (second children)))))

; Expression <- Sequence (Spacing* '/' SP* Sequence)*
(defnode expression 
  `(or-expr 
     ,@(remove-if #'not (mapcar 
      (lambda (el)
        (if (eql (match-kind el) :sequence-expr)
            (gen-sequence-expr el)))
      (match-children node)))))

; Sequence <- Rule (Spacing Rule)*
(defnode sequence-expr 
  (let 
    ((rules (remove-if 
         #'not 
         (mapcar 
           (lambda (el)
             (if (eql (match-kind el) :sequence-expr)
                 (gen-rule el)))
           (match-children node)))))
    `(compose ,@rules)))
     
; Rule <- PosLook / NegLook / Plain
(defnode rule
   (with-child (:child-kind t)
    (trivia:ematch kind
      (:pos-look (gen-pos-look child))
      (:neg-look (gen-neg-look child))
      (:plain (gen-plain child)))))

; Plain <- Primary Quant?
(defnode plain
  (let* ((primary (first (match-children node)))
         (quant (second (match-children node)))
         (quant-kind (and quant (match-kind quant))))
    (trivia:match quant-kind
      (:optional `(optional ,(gen-primary primary)))
      (:min-zero `(min-zero ,(gen-primary primary)))
      (:min-one `(min-one ,(gen-primary primary)))
      (:min-max-amount 

        
        `(gen-min-one ,(gen-primary primary)))

      (:amount `(min-one ,(gen-primary primary)))
      (_ (gen-primary primary)))))

(defnode min-max-amount
  (print "booya")
  )


; Primary <- Simple / CheckId / '(' Expression ')'
(defnode primary
  (with-child (:child-kind t)
    (trivia:ematch kind
      (:simple (gen-simple child))
      (:check-id (gen-check-id child))
      (:expression (gen-expression child)))))
  ; #+5am
  ; (5am:test gen-primary-test
  ;   (let ((primary-func
  ;           (eval 
  ;             (gen-primary
  ;               (parse #'primary "((alphanum '-' alphanum) / alphanum)")))))
  ;     (5am:is (funcall primary-func (coerce "4-a" 'list) 0))  
  ;     (5am:is (funcall primary-func (coerce "H" 'list) 0))  
  ;     (5am:is (funcall primary-func (coerce "9-9" 'list) 0))
  ;     (5am:is (eq NIL 
  ;                 (funcall primary-func (coerce "|" 'list) 0)))
  ;     (5am:is (eq NIL 
  ;                 (funcall primary-func (coerce "!" 'list) 0)))))

(defnode check-id 
  (with-node-literal (intern (string-upcase node-literal))))

(defnode simple
  (with-child (:child-kind t) 
    (trivia:ematch kind
      (:unicode (gen-unicode child))
      (:range-expr (gen-range-expr child))
      (:string-literal (gen-string-literal child))
      (:alphanum-class (gen-alphanum-class child))
      (:alpha-class (gen-alpha-class child))
      (:numeric-class (gen-numeric-class child)))))
#+5am
(5am:test gen-simple-test
  (let ((simple-func
          (eval 
            (gen-simple
              (parse #'simple "[`~A-Zf-i]")))))
    (5am:is (funcall simple-func (coerce "~hooo" 'list) 0))  
    (5am:is (funcall simple-func (coerce "H" 'list) 0))  
    (5am:is (funcall simple-func (coerce "g" 'list) 0))
    (5am:is (eq NIL 
                (funcall simple-func (coerce "88" 'list) 0)))
    (5am:is (eq NIL 
                (funcall simple-func (coerce "j" 'list) 0)))))

(defnode unicode
  (with-node-literal
   `(char-literal 
     ,(code-char 
        (read-from-string
          (format nil "#x~a" (remove #\u node-literal)))))))
#+5am
(5am:test gen-unicode-test
  (5am:is
   (funcall 
     (eval 
       (gen-unicode (parse #'unicode #?"u007A")))
     (coerce "zunky" 'list) 0)))

(defnode range-expr
  `(or-expr
      ,@(mapcar
          (lambda (child)
            (trivia:ematch child
              ((match (kind :any-char))
               (gen-any-char child))
              ((match (kind :char-range-literal))
               (gen-char-range-literal child))))
          (match-children node))))
(5am:test gen-range-expr-test
  (5am:is
   (funcall 
            (eval 
              (gen-range-expr (parse #'range-expr "[`A-Z0-9]")))
            (coerce "6IX" 'list) 0))
  (let ((da-funk
           (eval 
              (gen-range-expr (parse #'range-expr "[`A-Z0-9]")))))
    (5am:is (funcall da-funk (coerce "SIX" 'list) 0))
    (5am:is (funcall da-funk (coerce "`IX" 'list) 0))))

(defnode any-char
  (with-node-literal 
  `(char-literal ,(char node-literal 0))))
#+5am
(5am:test any-char-test
  (5am:is 
   (funcall 
     (eval 
       (gen-any-char (parse #'any-char "`00A")))
     (coerce "`[A-Z]" 'list) 0)))

(defnode alphanum-class
  `(or-expr (char-range #\A #\Z)
            (char-range #\a #\z)
            (char-range #\0 #\9)))
#+5am
(5am:test alphanum-class-test
  (5am:is 
   (funcall (eval 
              (gen-alphanum-class
                (parse #'alphanum-class "alphanum")))
            (coerce "38" 'list) 0))
  (5am:is 
   (eq NIL 
       (funcall (eval 
              (gen-alphanum-class
                (parse #'alphanum-class "alphanum")))
            (coerce "|38" 'list) 0))))

(defnode alpha-class
  `(or-expr (char-range #\A #\Z)
            (char-range #\a #\z)))
#+5am
(5am:test alpha-class-test
  (5am:is 
   (funcall (eval 
              (gen-alpha-class
                (parse #'alpha-class "alpha")))
            (coerce "a38" 'list) 0))
  (5am:is 
   (eq NIL 
       (funcall (eval 
              (gen-alpha-class
                (parse #'alpha-class "alpha")))
            (coerce "38" 'list) 0))))

(defnode numeric-class
  `(char-range #\0 #\9))
#+5am
(5am:test numeric-class-test
  (5am:is 
   (funcall (eval 
              (gen-numeric-class
                (parse #'numeric-class "numeric")))
            (coerce "38a" 'list) 0))
  (5am:is 
   (eq NIL 
       (funcall (eval 
              (gen-numeric-class
                (parse #'numeric-class "numeric")))
            (coerce "a38a" 'list) 0))))

(defnode char-range-literal
  (with-node-literal 
    `(char-range ,(char node-literal 0) 
                 ,(char node-literal 2))))
(gen-char-range-literal (parse #'char-range-literal "0-5"))
#+5am
(5am:test char-range-literal-test
  (5am:is 
   (funcall (eval 
              (gen-char-range-literal 
                (parse #'char-range-literal "0-5")))
            (coerce "38" 'list) 0))
  (5am:is 
   (eq NIL 
       (funcall (eval 
                  (gen-char-range-literal 
                    (parse #'char-range-literal "0-5")))
                (coerce "88" 'list) 0))))

(defnode string-literal 
  (with-node-literal 
    `(string-expr ,(remove #\' node-literal))))

(defun generate (file)
  (gen-grammar (parse-grammar file)))

#+nil
(gen-string-literal (parse #'string-literal "'boom'"))

#+nil
(generate (pathname #p"grammars/basic.peg"))
