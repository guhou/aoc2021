# Advent of Code 2021

[@guhou's](https://guhou.dev/) solutions to
[Advent of Code](https://adventofcode.com/) 2021. Implemented using
[Haskell](https://www.haskell.org/). My objectives when implementing solutions
to the puzzles provided is to explore Haskell libraries that I have not used
effectively in the past, namely
[`conduit`](https://hackage.haskell.org/package/conduit) and
[`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative).
I have not placed a strong emphasis on correctness, performance, readability, or
any other desirable characteristics in this code. It's just some code written
for the joy of participating in AoC.

## Requirements:

- [`ghc`](https://www.haskell.org/ghc/). Tested with 8.10.7
- [`cabal-install`](https://cabal.readthedocs.io/). Tested with 3.4.0.0

## Incantations:

- `cabal build` to build the library and executable.
- `cabal build aoc2021` to run the executable.
- `cabal test` to run the tests (if there were any).
