# cl-peg-yapp

[![CI/testing](https://github.com/maxArturo/cl-peg-yapp/actions/workflows/main.yml/badge.svg)](https://github.com/maxArturo/cl-peg-yapp/actions/workflows/main.yml)

> *Stands for "yet another PEG parser".*

This is a hand-rolled implementation of a [PEG](https://bford.info/packrat/) 
parsing generator from
first principles, for my own edification. The main objectives are parser
generation: a provided PEG input should result in a generated parser,
which can scan and parse a valid language sample.

It's heavily inspired from [cl-peg](https://github.com/thezerobit/cl-peg/tree/master)
for the end-user API, but with the difference that hopefully it is more easily readable, via
a liberal use of macros and no reliance on CLOS. Tests are also included in-line.

It can also expose and pretty-print the resulting node tree, with the
PEG expression definitions as a unified struct, called a `match`.
The current implementation also makes use of packrat caching as my uses
lean heavily towards memory tradeoff for speed.

Please note the flavor of PEG implemented is very basic and opinionated. The main caveats are:

- expression definitions always are CamelCase, starting with upcase.
- unicode literals (e.g. `u10EA44`) can be parsed, but other literals
  (binary, etc) are not implemented.
- definition arrows are single-dash, but both a unicode and compound arrow is accepted.
- String literals, (`'0'`, `"0"`), escaped chars (via `\`) and wildcard chars (`.`) are supporte

I'm considering making the definitions more flexible so anyone can bring their PEG definitions but for now it's not a priority. However, the
current implementation is tested against a flavor of the venerable [c-based markdown PEG in 'leg'](https://github.com/jgm/peg-markdown/blob/master/markdown_parser.leg)
and that seems to work fine.

## Usage

The external API is a bit clunky, but the tradeoff is that it gives you more insights into your grammar. Consider this example:

- You have a markdown definition, say like [this one](./src/tests/grammars/markdown.peg)
- You'd like load the def to parse out some markdown

You would accomplish that with this code:

```lisp
;; load the system

(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-peg-yapp")

;; load your grammar
(setf *my-grammar* 
  (cl-peg-yapp:parse-grammar #p"src/tests/grammars/markdown.peg"))

;; here you'll see the node structure of your grammar, e.g.
;; #(M :KIND :PATTERN :START 0000 :END 20261 matched str: >>>|Doc <-       BOM? Block* `Newline`  `Newline` Block <-     BlankLine* (BlockQuote `Newline`               /  Verbatim `Newline`               /  Note `Newline`               /  Reference `Newline`               /  HorizontalRule `Newline`               /  Heading `Newline`               /  OrderedList `Newline` ...  

;; now you can check if your nodes are correct and so on, or parse your target string via a generated parser function:

(setf *my-grammar-parser* (cl-peg-yapp:generate *my-grammar*))
(funcall *my-grammar-parser* #p"my-file.md")

;; you'll also get a node tree structure representing your parsed grammar:
;;
;; #(M :KIND :DOC :START 0000 :END 0043 matched str: >>>|## Hey  `Newline`  `Newline` This is me, [my link](example.com)|<<<:CHILDREN
;;    #(M :KIND :BLOCK :START 0000 :END 0008 matched str: >>>|## Hey  `Newline` |<<<:CHILDREN
;;       #(M :KIND :HEADING :START 0000 :END 0008 matched str: >>>|## Hey  `Newline` |<<<:CHILDREN
;;          #(M :KIND :ATXHEADING :START 0000 :END 0008 matched str: >>>|## Hey  `Newline` |<<<:CHILDREN
;;             #(M :KIND :ATXSTART :START 0000 :END 0002 matched str: >>>|##|<<<)
;;             #(M :KIND :SP :START 0002 :END 0003 matched str: >>>| |<<<:CHILDREN
;;                #(M :KIND :SPACECHAR :START 0002 :END 0003 matched str: >>>| |<<<))

```

## Testing

There's a considerable amount of tests available for the project. To run:

```lisp
;; load 5am
(ql:quickload "fiveam")

;; load the system
(ql:quickload "cl-peg-yapp")

;; run all the tests
(5am:run! 'cl-peg-yapp:peg-suite)
```
Tests are broken down by suites found in [test-suite.lisp](./src/test-suites.lisp)

