# j2j

Convert between jass files and jass-ast stored as json.

# Usage

Use `j2j to-json input.j` to print the jass-ast of input.j as json to stdout and
use `j2j to-jass input.json` to print the jass script represented by the
jass-ast stored json file `input.json` to stdout.

The json format is not yet documented and might be subject to change.

# Installation

You need a working haskell setup with ghc and cabal. Then clone this repo and
run `cabal build`. You can also use `cabal run j2j` to run it directly.
