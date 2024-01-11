(in-package #:cl-peg-yapp/peg-definition-tests)

(interpol:enable-interpol-syntax)

#+5am
(5am:def-suite* markdown-grammar-suite :in grammar-definition-suite)

(defmacro with-grammar-def (grammar-def &body body)
  `(progn 
     ,@(mapcar 
         (lambda (el)
           `(5am:test ,(first el)
              (5am:is 
               (test-match-literal 
                 ',(second el)
                 (funcall 
                   (generate (parse #'pattern ,grammar-def))
                   ,(third el)))
               )))
         body)))

(defmacro md-test (test-name test-obj test-str grammar-definition)
 `(5am:test ,test-name
     (let ((grammar ,grammar-definition))
       (5am:is 
        (test-match-literal 
          ,test-obj
          (funcall
            (generate 
              (parse #'pattern grammar))
            ,test-str))))))

(defparameter *md-spacing*
  #?"\
     \nNewLine <- (u000D u000A) / u000D / u000A
     \n# Space defined separately to use UTF char def
     \nSpace <- ' ' / u0009")
#+5am
(5am:def-suite* markdown-spacing-suite :in markdown-grammar-suite)

#+5am
(5am:test newline-test 
  (5am:is (test-match-literal 
            '(:newline)
            (funcall
              (generate 
                (parse #'pattern *md-spacing*))
              (coerce '(#\Newline) 'string)))))
 
(defparameter *md-foundation* 
  #?"\
     Text <- (!NewLine .)+ # TODO can be further refined
     \n${*md-spacing*}\n")

(defparameter *md-inline*
     #?|\
     Inline <- 
     \nImage /
     \nLink /
     \nStrong /
     \nEmphasis /
     \nText
     
     \nExclusionChars <- [*~]
     \nInnerText <- (!('*' / NewLine) .)+
     \nStrong <- '**' InnerText '**'
     \nEmphasis <- '*' InnerText '*'

     \nHyperlinkText <- (!']' .)+
     \nUrl <- (!(u0029 / Space) .)+ # closing right paren
     \nLink <- '[' HyperlinkText '](' Url ')'

     \nImageUrl <- (!(u0029 / u0022) .)+ # closing right paren 
     \n# or quotation mark closes image URL

     \nImageDescription <- (!u0022 .)+ # quotation mark
     \nImage <- '![' HyperlinkText '](' Url (' "' ImageDescription '"')? ')'\n|)

#+5am
(5am:def-suite* markdown-inline-suite :in markdown-grammar-suite)

#+5am
(with-grammar-def 
  #?"${*md-inline*}\n${*md-foundation*}"
  (link-test
    (:inline
      (:link
        (:hyperlinktext 
          "here's an image link")
        (:url
          "https://www.wikipedia.org/image.png")))
    "[here's an image link](https://www.wikipedia.org/image.png)")
  (text-test 
    (:inline
      (:text "Heyyall"))
    #?|Heyyall|)
  (image-test
    (:inline
      (:image 
        (:hyperlinktext 
          "here's an image link")
        (:url
          "https://www.wikipedia.org/image.png")
        (:imagedescription 
          "and this is what it looks like")))
    #?|![here's an image link](https://www.wikipedia.org/image.png \
    "and this is what it looks like")|)
  (strong-test
    (:inline
      (:strong 
        (:innertext
          "Heyyall, it's ya boy")))

    #?|**Heyyall, it's ya boy**|)
  (emphasis-test
    (:inline
      (:emphasis
        (:innertext
          "Heyyall, it's ya boy!! I think..   ")))
    #?|*Heyyall, it's ya boy!! I think..   *|))

(defparameter *md-heading* #?"\nHeading <- '#'{1,6} Space Text NewLine\n")

#+5am
(5am:def-suite* markdown-heading-suite :in markdown-grammar-suite)

#+5am
(with-grammar-def #?"${*md-heading*}\n${*md-foundation*}"
  (h1-test
    (:heading
      (:space)
      (:text "some heading")
      (:newline))
    #?"# some heading\n")
  (h6-test
    (:heading
      (:space)
      (:text "some heading!!!1! ###")
      (:newline))
    #?"###### some heading!!!1! ###\n"))

(defparameter *md-list* 
  #?"\nList <- (Ordered / Unordered) NewLine
     \nOrdered <- (Space '1.' / 
                       \nSpace '2.' / 
                       \nSpace '3.' / 
                       \nSpace '4.' / 
                       \nSpace '5.' / 
                       \nSpace '6.') Space Text

     \nUnordered <- Space '-' Space Text\n")
    
#+5am
(5am:def-suite* markdown-list-suite :in markdown-grammar-suite)

#+5am
(with-grammar-def #?"${*md-list*}\n${*md-foundation*}"
  (unordered-test
    (:list
      (:unordered
        (:space)
        (:space)
        (:text "my first list"))
      (:newline))
    #?" - my first list\n")
  (ordered-test-1
    (:list
      (:ordered
        (:space)
        (:space)
        (:text "my first list"))
      (:newline))
    #?" 1. my first list\n")
  (ordered-test-6
    (:list
      (:ordered
        (:space)
        (:space)
        (:text "my last list"))
      (:newline))
    #?" 6. my last list\n")
  )

(defparameter *md-blockquote* #?"\nBlockquote <- '>' Space Text NewLine \n")
#+5am
(5am:def-suite* markdown-blockquote-suite :in markdown-grammar-suite)

#+5am
(with-grammar-def #?"${*md-blockquote*}\n${*md-foundation*}"
  (blockquote-test
    (:blockquote
      (:space)
      (:text "some quote man... ")
      (:newline))
    #?"> some quote man... \n"))

(defparameter *md-codeblock*
  #?"\
     CodeBlock <- '```' NewLine CodeContent NewLine '```' NewLine
     \nCodeContent <- (!(NewLine '```') .)+\n")
#+5am
(5am:def-suite* markdown-codeblock-suite :in markdown-grammar-suite)

#+5am
(with-grammar-def #?"${*md-codeblock*}\n${*md-foundation*}"
  (basic-codecontent-test
    (:codeblock
      (:newline)
      (:codecontent
        "(defun foo (bar) (print bar))")
      (:newline)
      (:newline))
    #?"```\n(defun foo (bar) (print bar))\n```\n"))

#+5am
(5am:test extended-codecontent-test 
(let ((js-sample-code #?"\
 import jestCfg from './jest-esm.config.mjs';
 
 /** @type {import('ts-jest/dist/types').JestConfigWithTsJest} */
 const jestIsolatedCfg = {
   ...jestCfg,
   transform: {
     '^.+\\.(ts|js|html|svg)$': [
       'jest-preset-angular',
       {
         tsconfig: '<rootDir>/tsconfig-esm.spec.json',
         stringifyContentPathRegex: '\\.(html|svg)$',
         isolatedModules: true,
         useESM: true,
       },
     ],
   },
 };
 
 export default jestIsolatedCfg;"))
     (5am:is 
      (test-match-literal 
        (list :codeblock
              '(:newline)
              (list :codecontent #?"${js-sample-code}")
              '(:newline)
              '(:newline))
        (funcall (generate (parse #'pattern #?"${*md-codeblock*}\n${*md-foundation*}"))
                 #?"```\n${js-sample-code}\n```\n")))))

 


(defparameter *md-horizontal-rule* #?"\nHorizontalRule <- '-'{3} NewLine\n")

#+5am
(5am:in-suite markdown-grammar-suite)
#+nil
(TEST-MATCH-LITERAL 
  '(:horizontalrule
      (:newline))
  (FUNCALL
    (GENERATE
      (PARSE #'PATTERN
           #?"${*md-horizontal-rule*}\n${*md-foundation*}") )
     #?"---\n"))
#+5am
(with-grammar-def #?"${*md-horizontal-rule*}\n${*md-foundation*}"
  (horizontal-rule-test
    (:horizontalrule
      (:newline))
    #?"---\n"))

(defparameter *md-block*
     #?|\
     Block <- ${*md-heading*}\n
              \n${*md-list*} / 
              \n${*md-blockquote*} /
              \n${*md-codeblock*} /
              \n${*md-horizontal-rule*} /
              \n# \ Paragraph

     \nParagraph <- Text NewLine|)

(defparameter *md-grammar*
  ; #?"\
  ;    Markdown <- Heading
  ;    \n${*md-inline*}
  ;    \n${*md-foundation*}\n"
  #?"\
     Markdown <- Heading\n
     / Inline\n
     \n${*md-inline*}
     \n${*md-block*}
     \n${*md-foundation*}\n"
  )
     
#+nil
*md-grammar*

#+nil
(5am:def-suite* markdown-heading-suite :in markdown-grammar-suite)

#+nil
(md-test markdown-h1-test 
         '(:markdown 
            (:block
              (:heading
                (:space)
                (:headingtext
                  "my h1 heading")
                (:newline))))
         #?"# my h1 heading\n")

#+nil
(md-test markdown-h6-test 
          '(:markdown 
             (:block
               (:heading
                 (:space)
                 (:headingtext
                   "my h1 heading!!i!#")
                 (:newline))))
          #?"###### my h1 heading!!i!#\n")
#+nil
(funcall
  (generate 
    (parse #'pattern *md-grammar*) )
  #?"## my h1 
     ")
 
