# cl-peg-yapp

[![CI/testing](https://github.com/maxArturo/cl-peg-yapp/actions/workflows/main.yml/badge.svg?branch=main)](https://github.com/maxArturo/cl-peg-yapp/actions/workflows/main.yml)

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

Please note the flavor of PEG implemented is very basic and opinionated.
You can find it under `grammars/peg-simple.peg`. The main caveats are:

- expression definitions always are CamelCase, starting with upcase.
- unicode literals (e.g. `u10EA44`) can be parsed, but other literals
  (binary, etc) are not implemented.
- definition arrows are single-dash, but both a unicode and compound arrow is accepted.
- I'm considering making the definitions more flexible so anyone can bring their PEG
  definitions but for now it's not a priority.


