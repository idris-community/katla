---
title: Writing an Idris2 PDF file with katla-pandoc
idris2-packages:
  - contrib
colorlinks: true # Workaround for conflict between Katla and pandoc default latex template
---

Using [katla](https://github.com/idris-community/katla)'s pandoc backend we can produce PDF documents containing semantically highlighted Idris2 code.

The file you are currently reading is the rendered version of a literate markdown/idris2 file. It is called `Source.md` and contains fenced `idr` blocks.

# Basic Usage

We can hide uninteresting code blocks by adding the `hide` attribute. We hide an import of `Data.Vect` here. We won't see anything in the output PDF.

```{.idr .hide}
import Data.Vect
```

We can use all Idris 2 features. Here's an example code block:

```idr
x : Nat
x = 1 + 2
```

We can use namespaces by adding the `namespace` attribute, such as to provide alternate definitions for functions. Consider this function signature:

```{.idr namespace="A"}
f : Vect n a -> Vect (n + n) a
```

Here's one implementation for `f`{.idr namespace="A" type="Vect 1 Nat -> ?"}:

```{.idr namespace="A"}
f xs = xs ++ xs
```

By repeating the signature in a different namespace, in a hidden code block, I can give an alternate definition:

```{.idr .hide namespace="B"}
f : Vect n a -> Vect (n + n) a
```

```{.idr namespace="B"}
f xs = xs ++ reverse xs
```

# Inline Code

As well as block code, we can have inline code, like `the Nat 3`{.idr}. Here's another: `1 + 2`{.idr}. We can also call the function we previously defined: `f [1, 2, 3]`{.idr namespace="A"}.

By default, a code block is interpreted as top-level Idris 2 declarations, while inline code is interpreted as an expression. But we can switch it up if we wish.

By using the `decls` class, we can write an inline declaration: `g : Nat -> Nat`{.idr .decls}.

By using the `expr` class, we can write a block expression:

```{.idr .expr type="List Nat"}
[
    1,
    2,
    3
]
```

# Types

Sometimes, Idris 2 cannot infer the type of an expression. Is `[1, 2, 3]`{.idr type="List Nat"} a `List Nat`{.idr}, or a `Vect 3 Integer`{.idr}? We can use the `type` attribute to tell Idris 2 which one it should be.

# Multiple Files

If you need an import to *not* be available, namespaces are not enough. We can use the `file` attribute to put snippets in separate files. For example, the signature of `f`{.idr namespace="A" type="Vect 1 Nat -> ?"} fails to compile if we use a new file, that hasn't imported `Vect`{.idr}.

```{.idr file="Another"}
failing "Undefined name Vect."
    f : Vect n a -> Vect (n + n) a
```

# Packages

We can add Idris 2 packages to the YAML metadata of the Markdown file, using the `idris2-packages` key. For example, adding `contrib` allows us to use the `Language.JSON` module.

```{.idr file="Package"}
import Language.JSON

x : Maybe JSON
x = parse #"{"x": 1, "y": 2}"#
```
