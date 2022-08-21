# `yagi`

`yagi` is an experimental, dependently-typed programming language, still in the very early stages.  The project consists of three parts:

* `yagi-lang` defines the `yagi` syntax, parser, and typechecker
* `yagi-lsp-server` is a language server for `yagi` built with the [`lsp`](https://hackage.haskell.org/package/lsp) package
* `yagi-lsp-client` is `vscode` extension which communicates with the `yagi` language server to show parsing and type information

## Attributions

At the moment, significant portions of the code are directly adapted from the following excellent projects.

* For the intial typechecker, I translated some of the OCaml code from Andrej Bauer's series ["How to Implement Dependent Type Theory"](http://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/) into Haskell.

* The `yagi` parser is built with [`megaparsec`](https://markkarpov.com/tutorial/megaparsec.html#parsect-and-parsec-monads).  I am using a custom [`Stream`](https://markkarpov.com/post/megaparsec-more-speed-more-power.html#there-is-hope) instance to attach position information to each node of the tree.

* The structure of the `yagi` language server is mostly copied from [`dhall-lsp-server`](https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-lsp-server).  I learned a lot reading through the source code of the Dhall language server (also using `lsp`) and parser (also based on `megaparsec`).

* The `yagi` language client was written using [`vscode-haskell`](https://github.com/haskell/vscode-haskell) as a reference.  See also the [language server extension guide](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide).

The [`haskell-language-server`](https://github.com/haskell/haskell-language-server) source has also been a valuable reference.

## Features

Implemented

- [x] pi-types
- [x] universes
- [x] type annotations

Todo

- [ ] normalization by evaluation
- [ ] sigma-types
- [ ] pattern matching
- [ ] tactics
- [ ] typeclasses
- [ ] do-notation
- [ ] inductive types

Stretch Goals

- [ ] ornaments
- [ ] built-in induction principles and recursion schemes
- [ ] stream fusion