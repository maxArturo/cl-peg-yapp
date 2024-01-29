;;;; entrypoint for generating a full parser based
;;;; on a provided PEG spec.

(in-package #:cl-peg-yapp/peg-generator)
(interpol:enable-interpol-syntax)

#+5am
(5am:def-suite* parser-generation-suite :in generator-suite)

(defparameter *peg-node-ancestry* nil)
(defparameter *peg-entrypoint-definition* nil)

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

(defmacro with-child (&body body)
  `(let* ((child (first (match-children node)))
          (kind (match-kind child)))
     ,@body))

(defun generate (root-node &key symbols-only)
  (let*
    ((*peg-entrypoint-definition* nil)
     (grammar (gen-pattern root-node))
     (expr 
       `(lambda (input-str)
          (declare (string input-str))
          (let ((input (coerce input-str 'list)))
            (labels
              (,@grammar) 
              (,(intern *peg-entrypoint-definition*) input 0))))))
    (if symbols-only expr
        (eval expr))))
#+nil
(let ((input (parse #'pattern
              #?"\
# this grammar was taken off of wikipedia.
# see https://en.wikipedia.org/wiki/Parsing_expression_grammar#Examples
# for details.


Expr    ← Sum
Sum     ← Product (('+' / '-') Product)*
Product ← Power (('*' / '/') Power)*
Power   ← Value ('^' Power)?
Value   ← [0-9]+ / '(' Expr ')'")))
(generate input))

; Pattern         <- Exp !.
(defnode pattern
  (with-child 
    (trivia:ematch kind
      (:expression (gen-expression child)))))

; Exp             <- Spacing (Alternative / Grammar)
(defnode expression 
  (with-child 
    (trivia:ematch kind
      (:alternative (gen-alternative child))
      (:grammar (gen-grammar child)))))
#+5am
(5am:test gen-expression-test
  (5am:is
   (gen-expression (parse #'expression  "((alphanum '-' alphanum) / alphanum)"))))

; Alternative     <- Seq ('/' Spacing Seq)*
(defnode alternative
  (let ((children (match-children node)))
    (if (eql 1 (length children)) 
        (with-child 
          (trivia:ematch kind
            (:sequence-expr (gen-sequence-expr child))))
        `(or-expr 
           ,@(remove-if #'not (mapcar 
              (lambda (el)
                (if (eql (match-kind el) :sequence-expr)
                    (gen-sequence-expr el)))
              children))))))
 

(defnode grammar 
  `(,@(remove-if 
    #'not
    (mapcar 
      (lambda (el)
        (if (eql (match-kind el) :definition)
            (gen-definition el)))
      (match-children node)))))
#+nil
(gen-grammar 
  (parse #'grammar
              #?"\
Expr    ← Sum
Sum     ← Product (('+' / '-') Product)*
Product ← Power (('*' / '/') Power)*
Power   ← Value ('^' Power)?
Value   ← [0-9]+ / '(' Expr ')'"))

(defnode definition
  (let* 
    ((children (match-children node))
     (def-symbol
       (let ((node (first children)))
         (with-node-literal 
           (intern 
             (format nil "~:@(~a~)" node-literal)
             "KEYWORD"))))
     (def-id (gen-name (first children))))
    `(,def-id
       (input index)
       (declare (list input) (fixnum index))
       (let* ((result 
                (with-caching 
                  (format nil "~A-~A" ,def-symbol index)
                  (funcall 
                    ,(gen-expression (second children)) 
                    input index)))
              (result-match 
                (and result
                     (new-match 
                       input index (match-end result) result
                       ,def-symbol))))
         (compact-match result-match)))))
#+nil
(gen-definition 
    (parse #'definition "AddExpr  <- ('+'/'-') Factor"))

; Seq             <- Prefix+
(defnode sequence-expr 
  (let 
    ((rules
       (mapcar 
         (lambda (el)
           (trivia:ematch el
             ((match (kind :pos-look))
              (gen-pos-look el))
             ((match (kind :neg-look))
              (gen-neg-look el))
             ((match (kind :suffix))
              (gen-suffix el))))
           (match-children node))))
    `(compose ,@rules)))
     
; Suffix          <- Primary Spacing (Quant Spacing)?
(defnode suffix
  (let* ((primary (first (match-children node)))
         (quant (second (match-children node)))
         (quant-base (and quant (first (match-children quant))))
         (quant-kind (and quant-base (match-kind quant-base))))
    (trivia:match quant-kind
       (:optional `(opt-expr ,(gen-primary primary)))
       (:min-zero `(zero-or-more ,(gen-primary primary)))
       (:min-one `(one-or-more ,(gen-primary primary)))
       (:min-max-amount 
         (let ((min-amt (first (match-children quant-base)))
               (max-amt (second (match-children quant-base))))
           `(min-max-times 
              ,(gen-primary primary) 
              ,(gen-min-amount min-amt)
              ,(gen-max-amount max-amt))))
       (:amount 
         (let ((exact-amt (first (match-children quant-base))))
           `(times ,(gen-primary primary) 
                   ,(gen-exact-amount exact-amt))))
       (_ (gen-primary primary)))))
#+5am
(5am:test gen-suffix-test
  (5am:is 
   (eq
     85
     (nth 3
       (gen-suffix
         (parse #'suffix "'#'{83,85}"))))))

; PosLook    <- '&' Suffix
(defnode pos-look
  (with-child 
    (trivia:ematch kind
      (:suffix
        `(positive-lookahead ,(gen-suffix child))))))

; NegLook <- '!' Suffix
(defnode neg-look
  (with-child 
    (trivia:ematch kind
      (:suffix
        `(negative-lookahead ,(gen-suffix child))))))

(defnode exact-amount
  (with-node-literal `,(parse-integer node-literal)))
#+5am
(5am:test gen-min-amount-test
  (5am:is 
   (eql
     83
     (gen-exact-amount
       (first 
         (match-children 
           (parse #'amount "{83}")))))))

(defnode min-amount
  (with-node-literal `,(parse-integer node-literal)))
#+5am
(5am:test gen-min-amount-test
  (5am:is 
   (eql
     83
     (gen-min-amount 
       (first 
         (match-children 
           (parse #'min-max-amount "{83,85}")))))))

(defnode max-amount
  (with-node-literal `,(parse-integer node-literal)))
#+5am
(5am:test gen-min-amount-test
  (5am:is 
   (eql
     85
     (gen-max-amount 
       (second
         (match-children 
           (parse #'min-max-amount "{83,85}")))))))

; Primary <- Simple / Name / '(' Expression ')'
(defnode primary
  (with-child
    (trivia:ematch kind
      (:simple (gen-simple child))
      (:name (gen-name child))
      (:expression (gen-expression child)))))
 #+5am
 (5am:test gen-primary-test
   (let ((primary-func
           (eval 
             (gen-primary
               (parse #'primary "((alphanum '-' alphanum) / alphanum)")))))
     (5am:is (funcall primary-func (coerce "4-a" 'list) 0))  
     (5am:is (funcall primary-func (coerce "H" 'list) 0))  
     (5am:is (funcall primary-func (coerce "9-9" 'list) 0))
     (5am:is (eq NIL 
                 (funcall primary-func (coerce "|" 'list) 0)))
     (5am:is (eq NIL 
                 (funcall primary-func (coerce "!" 'list) 0)))))

(defnode name
  (with-node-literal 
    (let* ((upcase-id (format nil "~:@(~a-def~)" node-literal))
           (def-id (intern upcase-id)))
    (if (not *peg-entrypoint-definition*)
        (setf *peg-entrypoint-definition* 
             upcase-id ))  
    (if (find :alternative *peg-node-ancestry*) 
        `#',def-id
        def-id))))

(defnode simple
  (with-child
    (trivia:ematch kind
      (:unicode (gen-unicode child))
      (:range-expr (gen-range-expr child))
      (:string-literal (gen-string-literal child))
      (:alphanum-class (gen-alphanum-class child))
      (:alpha-class (gen-alpha-class child))
      (:numeric-class (gen-numeric-class child))
      (:wildcard-class (gen-wildcard-class child)))))
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
       (gen-unicode (parse #'unicode "u000A")))
     '(#\Newline) 0))
  (5am:is
   (funcall 
     (eval 
       (gen-unicode (parse #'unicode "u007A")))
     (coerce "zunky" 'list) 0)))

(defnode range-expr
  `(or-expr
      ,@(mapcar
          (lambda (child)
            (trivia:ematch child
              ((match (kind :range-char-option))
               (gen-range-char-option child))
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

(defnode range-char-option
  (if (match-children node)
      (with-child 
        (trivia:ematch kind
          (:escaped-square-bracket
            `(char-literal #\]))))
      (with-node-literal
        `(char-literal ,(char node-literal 0)))))
#+5am
(5am:test range-char-option-test
  (5am:is 
   (funcall 
     (eval 
       (gen-range-char-option (parse #'range-char-option "\\]")))
     (coerce "]" 'list) 0))
  (5am:is 
   (funcall 
     (eval 
       (gen-range-char-option (parse #'range-char-option "`00A")))
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

(defnode wildcard-class
  `(or-expr #'any-char))
#+5am
(5am:test wildcard-class-test
  (5am:is (test-input 
            (eval 
              (gen-wildcard-class
                (parse #'wildcard-class ".")))
            "a38"))
  (5am:is 
   (eq NIL 
       (test-input 
            (eval 
              (gen-wildcard-class
                (parse #'wildcard-class "."))) 
            ""))))

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
    (if (match-children node)
        (with-child 
          (trivia:ematch kind
            (:escaped-hyphen
              (if (eql (match-start child) node-start)
                  `(char-range #\- 
                               ,(char node-literal 3))   
                  `(char-range ,(char node-literal 0)
                               #\-)))))
        `(char-range ,(char node-literal 0) 
                     ,(char node-literal 2)))))
#+5am
(5am:test char-range-literal-test
  (5am:is 
   (funcall (eval 
              (gen-char-range-literal 
                (parse #'char-range-literal "\\--5")))
            (coerce "38" 'list) 0))
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
    (let* ((string-delim (char node-literal 0))
           (stripped-str 
             (regex-replace-all
               (format nil "\\\\~c" string-delim)
                           node-literal 
                           (format nil "~c" string-delim))))
      `(string-expr ,(subseq stripped-str 1 (1- (length stripped-str)))))))
#+5am
(5am:test string-literal-test
  (5am:is 
   (funcall (eval 
              (gen-string-literal
                (parse #'string-literal "'most\\'ly'")))
            (coerce "most'ly" 'list) 0))
  (5am:is 
   (funcall (eval 
              (gen-string-literal
                (parse #'string-literal "\"yo\\\"ma\"")))
            (coerce "yo\"ma" 'list) 0))
  (5am:is 
   (eq NIL 
       (funcall (eval 
                  (gen-char-range-literal 
                    (parse #'char-range-literal "0-5")))
                (coerce "88" 'list) 0))))
