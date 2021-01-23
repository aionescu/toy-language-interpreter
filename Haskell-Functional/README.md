# tli/Haskell-Functional

A functional flavor of Toy Language, implemented in Haskell.

## Building & running

To build the project, you will need `cabal`, which can be found [here](https://www.haskell.org/platform/).

To build, run `cabal new-build` in the `Haskell-Functional` folder once you've cloned the repo.

You can then either install the package globally running `cabal install`, or run the local build with `cabal new-run` (e.g. `cabal new-run . -- run Examples/Code.tl`).

Usage (assuming you `cabal install`ed the project):

```sh
tli run <path>
tli dump-ast [--no-type-check] <path>
```
