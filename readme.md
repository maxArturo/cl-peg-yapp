# cl-peg-yapp

This is a hand-rolled implementation of a PEG parsing generator from first principles. 
The main objectives are parser generation: a provided PEG input should result in a 
generated parser, which can scan and parse a valid language sample.

It can also expose the resulting node tree, with the expression definitions as one kind
of struct and "terminals" (the literal characters of the source text) as another struct.

Please note the flavor of PEG implemented is very basic and opinionated. The main caveats are:
- expression definitions always are CamelCase, starting with upcase.
- unicode literals (e.g. `u10EA44`) can be parsed, but other literals (binary, etc) are not implemented.
- definition arrows are single-dash

