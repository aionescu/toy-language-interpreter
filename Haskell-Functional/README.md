# tli/Haskell

Haskell version of the interpreter.

## Building & running

To build the project, you will need `cabal`, which can be found [here](https://www.haskell.org/platform/).

To build, run `cabal new-build` in the `Haskell` folder once you've cloned the repo.

You can then either install the package globally running `cabal install`, or run the local build with `cabal new-run` (e.g. `cabal new-run . -- run Examples/Code.tl`).

Usage (assuming you `cabal install`ed the project):

```sh
tli run [--small-step] [--fs-root <path>] [--max-heap <int>] [--gc-threshold <int>] <path>
tli dump-ast [--no-type-check] <path>
```
