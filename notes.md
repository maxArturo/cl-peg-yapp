# Useful dev notes

## Code layout

- `grammars`: contains PEG files
- `src/parser`: functions that represent PEG-based patterns (one or
  more, negative lookup, etc)
- `src/peg-grammar`: an implementation of a PEG parser using the above
- `src/generator`: the PEG parser generator which
  - reads a PEG definition 
  - builds a lexical function from the grammar `match`
  - merges it and runs `eval` for end user consumption


## Parser

There are only `match`es for our purpose. They map to traditional
PEG definitions like so:

- parents: `match`es with `:children`

- terminals: `match`es without `:children`

Consider a word. It is composed of one or more of:

- character, e.g. `\[A-Za-z\]`
- a word terminating char, which won't be consumed

This would be matched for us like so:

```
input: "hello!"

output:

#(M :KIND :NIL :START 0000 :END 0005 matched str: >>>| hello |<<<)

```

