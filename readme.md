# cl-peg-yapp

> *Stands for "yet another PEG parser".*

This is a hand-rolled implementation of a [PEG](https://bford.info/packrat/) 
parsing generator from
first principles, for my own edification. The main objectives are parser
generation: a provided PEG input should result in a generated parser,
which can scan and parse a valid language sample.

It can also expose and pretty-print the resulting node tree, with the
PEG expression definitions as one kind of struct and "terminals" (the
literal characters of the source text) as another struct. I'm also
considering making this a packrat parser as well, given its speed
improvements. But it's not a top priority.

Please note the flavor of PEG implemented is very basic and opinionated.
You can find it under `grammars/peg-simple.peg`. The main caveats are:

- expression definitions always are CamelCase, starting with upcase.
- unicode literals (e.g. `u10EA44`) can be parsed, but other literals
  (binary, etc) are not implemented.
- definition arrows are single-dash, but both a unicode and compound arrow is accepted.


