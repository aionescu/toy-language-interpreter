# toy-language-interpreter

Toy Language Interpreter, written for the Advanced Programming Methods course @ Babes-Bolyai University.

## Contents

* [Java](Java): Java implementation of the interpreter, as required by the course. Also has a parser and a few extra features:
  * [Tuples](Java/Examples/Tuple.tl)
  * [Records](Java/Examples/Record.tl)
  * [Lambda expressions](Java/Examples/Lambda.tl)
* [Haskell](Haskell): Haskell implementation of the interpreter. Has all the features of the Java version, except for multithreading.
* [Haskell-Functional](Haskell-Functional): Haskell implementation of a functional flavor of Toy Language, which also supports [structural subtyping](Haskell-Functional/Examples/Subtyping.tl).
* [Rust](Rust): Just an AST definition and macro-based parser.
* [tl-vscode](tl-vscode): Very basic VS Code extension to provide syntax highlighting for Toy Language programs.

## Assignments & Tags

The assignment statements can be found in the [Assignments](Assignments) folder.

The commits that were presented during the labs are tagged with their corresponding assignment.

## License

This repository is licensed under the GNU General Public License v3.

For more details, see [the license file](LICENSE.txt).
