# Top Down Operator Precedence

This is a no-frills Haskell implementation of a Pratt parser, [as described by Douglas Crockford][0]. Given a string of code, it tokenizes it, then parses it into an abstract syntax tree. The parser implementation is completely decoupled from the rules for tokenizing and the AST data type, making it generally applicable. I drew upon this [excellent example][2] of a monadic Pratt parser in F#, although I think the Haskell solution has less complexity - possibly because the tokens are pre-processed into a list of symbols, rather than looking them up whilst parsing, which greatly simplifies error handling.

## Limitations/TODO
- Handling scope. Crockford's builds a tree of scopes as it goes, so probably will have to add something to InputState. This will make it uglier.

[0]: http://javascript.crockford.com/tdop/tdop.html
[1]: http://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing/
[2]: http://matthewmanela.com/blog/a-monadic-pratt-parser/
