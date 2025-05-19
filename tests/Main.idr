module Main

import System
import Test.Golden

%default covering

Pandoc : Requirement
Pandoc = MkReq "pandoc" $ do
    (_, 0) <- run "command -v pandoc"
        | _ => pure Nothing
    pure $ Just "pandoc"

baseTests : TestPool
baseTests = MkTestPool "Examples using Katla" [] Nothing
  [ "standalone"
  , "raw-snippet"
  , "preamble"
  , "config"
  , "macros"
  , "init"
  , "markdown"
  , "literate"
  ]

pandocTests : TestPool
pandocTests = MkTestPool "Examples using Katla-Pandoc" [Pandoc] Nothing
  [ "pandoc"
  ]

main : IO ()
main = runner
  [ withPath "examples" baseTests
  , withPath "examples" pandocTests
  ]

 where
   withPath : String -> TestPool -> TestPool
   withPath path pool = { testCases $= map (path ++ "/" ++) } pool
