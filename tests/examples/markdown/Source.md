# A markdown Idris file

We hide the module declaration because it is **booooring**

```idris hide
module Source
```

We are however quite proud to be working with total functions only:

```idris
%default total
```

Here is a failing block

```idris
failing "Can't find an implementation for FromString Nat"
  t : Nat
  t = "h"
```

And here is a successful definition

```idris
main : IO ()
main = putStrLn "Hello, but in Markdown"
```
