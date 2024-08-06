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

# Usage

To generate a document, a corresponding `ttm` file is required along with the `idr` file.
After a successful build, you will find them in `build/ttc/{ttc_version}`.
`ttc_version` depends on your environment.

To generate an HTML:

```console
$ katla html path/to/src/Foo.idr path/to/ttm/Foo.ttm > Foo.html
```

To generate a TeX file and a PDF:

```console
$ katla latex path/to/src/Foo.idr path/to/ttm/Foo.ttm > Foo.tex
$ pdflatex Foo.tex
```

Generated TeX files may have dependencies.
Currently, they require the following packages:

- inconsolata
- fancyvrb
- xcolor

You can investigate potential dependencies by searching the code:

```console
$ grep usepackage src/Katla/LaTeX.idr
```

Active examples of using Katla can be found in the main repository of Idris2:  
https://github.com/idris-lang/Idris2/blob/main/.github/scripts/katla.sh

# Demo
See [example tests](./tests/examples).

# PRs welcome!

Wishlist:

+ Error handling
+ Batch mode
+ Separate into a library + driver module (so we could write Idris programs that generate such sources)
