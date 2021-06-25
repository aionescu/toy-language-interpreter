# tli/Haskell-Functional

A functional flavor of Toy Language that uses structural subtyping, implemented in Haskell.

## Building & running

You will need `cabal`, which can be found [here](https://www.haskell.org/platform/).

To build, run the following:

```sh
cabal new-build
```

You can then either install the package globally:

```sh
cabal install

# And run with
tli Code.tl
```

Or you can directly run the local build:

```sh
cabal new-run . -- Code.tl
```
