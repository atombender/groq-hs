# groq-hs

A Haskell port of the [go-groq](https://github.com/sanity-io/go-groq) parser for the GROQ query language.

## Project Structure

- `src/Groq/AST.hs`: Abstract Syntax Tree definitions.
- `src/Groq/Parser.hs`: Megaparsec-based parser implementation.
- `example/Main.hs`: REPL application.
- `test/Main.hs`: Test suite using Hspec.

## Prerequisites

- [GHC](https://www.haskell.org/ghc/) (tested with 9.12.2)
- [Cabal](https://www.haskell.org/cabal/) (tested with 3.16.0.0)

## Build

```bash
cabal build
```

## Run REPL

To start the interactive Read-Eval-Print Loop:

```bash
cabal run
```

Example interaction:
```
> *[_type == "movie"]{title}
Projection {projLHS = Filter {filterLHS = Everything, filterConstraint = BinaryOperator {binOp = OpEquals, binLHS = Attribute "_type", binRHS = Literal (StringLiteral "movie")}}, projObject = Object [ObjectAttribute {fieldName = "title", fieldValue = Attribute "title"}]}
```

## Run Tests

To run the test suite:

```bash
cabal test
```
