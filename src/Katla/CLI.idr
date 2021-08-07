module Katla.CLI

import public Collie

import Katla.Config
import Katla.LaTeX
import Katla.Engine

%default covering

export
macroCmd : Command "macro"
macroCmd = MkCommand
  { description = """
      Generate a macro that typesets the code snippet
      """
  , subcommands = []
  , modifiers = []
  , arguments = lotsOf filePath
  }

export
katlaCmd : Command "katla"
katlaCmd = MkCommand
  { description = """
      Katla v0.1.
      LaTeX code listing generator for Idris2
      """
  , subcommands =
    [ "--help"   ::= basic "Print this help text." none
    , "preamble" ::= preambleCmd
    , "init"     ::= initCmd
    , "macro"    ::= macroCmd
    ]
  , modifiers   = ["--config" ::= option """
                    Preamble configuration file in Dhall format.
                    Use `init` to generate the defaults config file.
                    """ filePath
                  , "--snippet" ::= flag """
                    Generates a standalone LaTeX file when unset or just \
                    a code snippet when set.
                    Default: unset/false.
                    """
                  ]
  , arguments = lotsOf filePath
  }

rawSnippet : Bool -> Maybe Snippet
rawSnippet False = Nothing
rawSnippet True  = Just (Raw Nothing)

export
katlaExec : CLI.katlaCmd ~~> IO ()
katlaExec =
  [ \parsed => case parsed.arguments of
       Just [src, md, output] =>
         katla (rawSnippet $ parsed.modifiers.project "--snippet")
               (parsed.modifiers.project "--config")
               (Just src) (Just md) (Just output)
       Just [src, md]         =>
         katla (rawSnippet $ parsed.modifiers.project "--snippet")
               (parsed.modifiers.project "--config")
               (Just src) (Just md) Nothing
       _ => putStrLn katlaCmd.usage
  , "macro"   ::= [\parsed => case parsed.arguments of
      Just [name, src, md, output] =>
        katla (Just $ Macro (name, Nothing))
              Nothing
              (Just src) (Just md) (Just output)
      Just [name, src, md, output, offset, before, after] =>
        katla (Just $ Macro (name, Just $ MkListingRange
                                    { offset = cast offset - 1
                                    , before = cast before
                                    , after  = cast after}))
              Nothing
              (Just src) (Just md) (Just output)
      Just [name, src, md, offset, before, after] =>
        katla (Just $ Macro (name, Just $ MkListingRange
                                    { offset = cast offset - 1
                                    , before = cast before
                                    , after  = cast after}))
              Nothing
              (Just src) (Just md) Nothing

      Just [name, src, md] =>
        katla (Just $ Macro (name, Nothing))
              Nothing
              (Just src) (Just md) Nothing
      _ => putStrLn katlaCmd.usage
      ]
  , "--help"  ::= [const $ putStrLn katlaCmd.usage]
  , "preamble" ::= [preamble]
  , "init"     ::= [init]
  ]
