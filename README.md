# Katla: LaTeX & HTML code listing generator for Idris2

Pre-alpha version. Interface might change as the command line
interface generator library [Collie](https://github.com/ohad/collie)
evolves.

Dependencies:
+ Idris2 downstream of [PR#1941](https://github.com/idris-lang/Idris2/pull/1941)
+ [Installing](https://github.com/idris-lang/Idris2/blob/master/INSTALL.md#6-optional-installing-the-idris-2-api) the Idris 2 API
+ [Collie](https://github.com/ohad/collie) library for command line interfaces
+ [Idrall](https://github.com/alexhumphreys/idrall) library for
  [Dhall](https://dhall-lang.org/) bindings

# Demo
See [example tests](./tests/examples).

# PRs welcome!

Wishlist:

+ Error handling
+ Batch mode
+ Separate into a library + driver module (so we could write Idris programs that generate such sources)
