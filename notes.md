# Useful dev notes

## Code layout

- `grammars`: contains PEG files
- `src/parser`: functions that represent PEG-based patterns (one or
  more, negative lookup, etc)
- `src/peg-grammar`: an implementation of a PEG parser using the above

## Parser

There are two kinds of nodes

- parents

- terminals

Consider a word. It is composed of one or more of:

- character, e.g. `\[A-Za-z\]`
- a word terminating char, which won't be consumed

so when you call a parser your output will for sure be a list, maybe
like this:

'(#\h \#\e \#\y)

Additionally, if you're parsing something that hasn't consumed all the
input then it should return the remainder. Its all a list of chars
really. Maybe let's just make it explicit with a plist?

'(:result NIL :remainder '(...)) '(:result \#S(EXPRESSION :kind :word
:value '(#\a \#\h)) :remainder '(...))

also, successful parsings that do not consume input will be represented
as a list with a single NIL, and the input intact; eg.:

ncall funcall \#'peg-parser::negative-lookahead (funcall
'peg-parser::literal-char-terminal \#\i)) coerce "figaro" 'list)) =\>
(:RESULT (NIL) :REMAINDER (#\f \#\i \#\g \#\a \#\r \#\o)

The way we distinguish empty successful matches is with a symbol of
:empty-success

RE: the peg implementation
