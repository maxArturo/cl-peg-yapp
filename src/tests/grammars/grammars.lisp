(in-package #:cl-peg-yapp/peg-definition-tests)

(interpol:enable-interpol-syntax)
#+5am
(5am:def-suite* markdown-grammar-suite :in grammar-definition-suite)

(defmacro md-test (test-name test-obj test-str)
  `(5am:test ,test-name
    (5am:is 
     (test-match-literal 
       ,test-obj
       (funcall
         (generate 
           (parse #'pattern *md-grammar*))
         ,test-str)))))


(defparameter *md-foundation* 
  #?"\
     \nNewLine <- (u0013 u0010) / u0013 / u0010
     \n# Space defined separately to use UTF char def
     \nSpace <- ' ' / u0009
     \nText <- (!NewLine .)+ # TODO can be further refined")

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
     \nImage <- '![' HyperlinkText '](' Url (' "' ImageDescription '"')? ')'|)

(defparameter *md-block*
     #?|\
     Block <- Heading\n
              \n# \ List / 
              \n# \ Blockquote /
              \n# \ CodeBlock /
              \n# \ HorizontalRule /
              \n# \ Paragraph

     \nHeadingText <- (!(Space* '#') .)+
     \nHeading <- '#'{1,6} Space HeadingText (!Newline .)* NewLine

     \nList <- (Ordered / Unordered) NewLine

     \nOrdered <- (Space '1.' / 
                       \nSpace '2.' / 
                       \nSpace '3.' / 
                       \nSpace '4.' / 
                       \nSpace '5.' / 
                       \nSpace '6.') Space Text NewLine

     \nUnordered <- Space '-' Space Text NewLine

     \nBlockquote <- '>' Space Text NewLine

     \nCodeContent <- (Space / NewLine / [u0020-u007E])
     \nCodeBlock <- '```' CodeContent (!'```' CodeContent)+ '```' NewLine

     \nHorizontalRule <- '---' NewLine

     \nParagraph <- Text NewLine|)

(defparameter *md-grammar*
  ; #?"\
  ;    Markdown <- Heading
  ;    \n${*md-inline*}
  ;    \n${*md-foundation*}\n"
  #?"\
     Markdown <- Block / Inline\n
     \n${*md-inline*}
     \n${*md-block*}
     \n${*md-foundation*}\n"
  )
     
#+nil
*md-grammar*

#+5am
(5am:def-suite* markdown-heading-suite :in markdown-grammar-suite)
(md-test markdown-h1-test 
         '(:markdown 
            (:block
              (:heading
                (:space)
                (:headingtext
                  "hey"))))
         #?|## hey ##|)

#+5am
(5am:def-suite* markdown-inline-suite :in markdown-grammar-suite)
#+5am
(md-test markdown-link-test 
         '(:markdown 
            (:inline
              (:link
                (:hyperlinktext 
                  "here's an image link")
                (:url
                  "https://www.wikipedia.org/image.png"))))
         #?|[here's an image link](https://www.wikipedia.org/image.png)|)
#+5am
(md-test markdown-image-test
         '(:markdown 
            (:inline
              (:image 
                (:hyperlinktext 
                  "here's an image link")
                (:url
                  "https://www.wikipedia.org/image.png")
                (:imagedescription 
                  "and this is what it looks like"))))
         #?|![here's an image link](https://www.wikipedia.org/image.png "and this is what it looks like")|)
#+5am
(md-test markdown-text-test
         '(:markdown 
            (:inline
              (:text "Heyyall")))
         #?|Heyyall|)
#+5am
(md-test markdown-strong-test
         '(:markdown 
            (:inline
              (:strong 
                (:innertext
                  "Heyyall, it's ya boy"))))
         #?|**Heyyall, it's ya boy**|)
#+5am
(md-test markdown-emphasis-test
         '(:markdown 
            (:inline
              (:emphasis
                (:innertext
                  "Heyyall, it's ya boy!! I think..   "))))
         #?|*Heyyall, it's ya boy!! I think..   *|)
#+nil
(funcall
        (generate 
          (parse #'pattern *md-grammar*))
#?|**Heyyall, its ya boy!!! I think... **|)
 
