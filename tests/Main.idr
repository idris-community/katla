module Main

import Test.Golden

%default covering

tests : TestPool
tests = MkTestPool "Examples using Katla" [] Nothing
  [ "standalone"
  , "raw-snippet"
  , "preamble"
  , "config"
  , "macros"
  , "init"
  , "markdown"
  , "literate"
  , "pandoc"
  ]

main : IO ()
main = runner
  [ withPath "examples" tests
  ]

 where
   withPath : String -> TestPool -> TestPool
   withPath path pool = { testCases $= map (path ++ "/" ++) } pool
