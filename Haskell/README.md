# tli/Haskell

Haskell version of the interpreter.

## Building & running

To build the project, you will need `cabal`, which can be found [here](https://www.haskell.org/platform/).

To build, run `cabal new-build` in the `Haskell` folder once you've cloned the repo.

You can then either install the package globally running `cabal install`, or run the local build with `cabal new-run` (e.g. `cabal new-run . -- run Examples/Code.tl`).

Usage (assuming you `cabal install`ed the project):

```sh
tli run <path> # Runs the program and displays the final output.
tli run --small-step <path> # Runs and displays every intermediate program state.

tli dump-ast <path> # Parses, type-checks, then prints the AST of the program.
tli dump-ast --no-type-check <path> # Prints the AST straight from the parser.
```

If `-` is passed as the `<path>`, then the interpreter will read the code from stdin.
e.g. `echo 'a : _ <- 2; print a' | tli run -`
