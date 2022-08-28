# Tutorial: writing an Idris2 blog post

Using [katla](https://github.com/idris-community/katla)'s markdown backend
we can produce documents containing semantically highlighted Idris2 code.

The file you are currently reading is the rendered version of a literate
markdown/idris2 file. It is called `Source.md` and contains fenced `idris`
blocks. For instance the following code block declares the `Source` module.

```idris
module Source
```

It is easy to hide uninteresting code blocks. E.g. the following line contains
a code block importing `Data.String` which we have purposefully hidden by passing
the `hide` attribute to the `idris` fence.
```idris hide
import Data.String
```
But we decide to proudly display the fact all our definitions are total by default.
```idris
%default total
```

We can make use of all of the language's feature. E.g. we can write `failing` blocks
to illustrate invalid code. Because we have turned the totality checker on, we have
to write obviously terminating functions. The following function is for instance
rejected.

```idris
failing "non_structural_product is not total, possibly not terminating"

  non_structural_product : List Nat -> Nat
  non_structural_product []         = 1
  non_structural_product [x]        = x
  non_structural_product (x::y::xs) = non_structural_product (x*y::xs)
```

And here is a successful definition (which demonstrates that we have indeed imported
`Data.String` in a hidden block and thus have access to `unwords`).
```idris
main : IO ()
main = putStrLn
     $ (\x => x)
     $ unwords
     [ "Hello,"
     , "from"
     , "the"
     , "Markdown"
     , "mode"
     ]
```
