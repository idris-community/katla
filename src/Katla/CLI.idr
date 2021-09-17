module Katla.CLI

import public Collie

import Katla.Config
import Katla.LaTeX
import Katla.HTML
import Katla.Engine

%default covering

export
inlineCmd : Command "inline"
inlineCmd = MkCommand
  { description = """
      Generate a macro that typesets an inline code snippet
      """
  , subcommands = []
  , modifiers = []
  , arguments = lotsOf filePath
  }

export
macroCmd : Command "macro"
macroCmd = MkCommand
  { description = """
      Generate a macro that typesets the code snippet
      """
  , subcommands = ["inline" ::= inlineCmd]
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
         katla HTML.mkDriver
               (rawSnippet $ parsed.modifiers.project "--snippet")
               (parsed.modifiers.project "--config")
               (Just src) (Just md) (Just output)
       Just [src, md]         =>
         katla HTML.mkDriver
               (rawSnippet $ parsed.modifiers.project "--snippet")
               (parsed.modifiers.project "--config")
               (Just src) (Just md) Nothing
       _ => putStrLn katlaCmd.usage
  , "macro"   ::=
    [\parsed => case parsed.arguments of
      Just [name, src, md, output] =>
        katla HTML.mkDriver
              (Just $ Macro (name, False, Nothing))
              Nothing
              (Just src) (Just md) (Just output)
      Just [name, src, md, output, offset, before, after] =>
        katla HTML.mkDriver
              (Just $ Macro (name, False, Just $ RowRangeByOffset
                                    { offset = cast offset - 1
                                    , before = cast before
                                    , after  = cast after}))
              Nothing
              (Just src) (Just md) (Just output)
      Just [name, src, md, offset, before, after] =>
        katla HTML.mkDriver
              (Just $ Macro (name, False, Just $ RowRangeByOffset
                                    { offset = cast offset - 1
                                    , before = cast before
                                    , after  = cast after}))
              Nothing
              (Just src) (Just md) Nothing

      Just [name, src, md] =>
        katla HTML.mkDriver
              (Just $ Macro (name, False, Nothing))
              Nothing
              (Just src) (Just md) Nothing
      _ => putStrLn katlaCmd.usage
    , "inline" ::= [\parsed => case parsed.arguments of
      Just [name, src, md, output, offset, line, startCol, endCol] =>
        katla HTML.mkDriver
              (Just $ Macro (name, True, Just (RangeByOffsetAndCols
                                    { offset = cast offset - 1
                                    , after  = cast line
                                    , startCol  = cast startCol - 1
                                    , endCol    = cast endCol
                                    })))
              Nothing
              (Just src) (Just md) (Just output)
      Just [name, src, md,         offset, line, startCol, endCol] => do
        katla HTML.mkDriver
              (Just $ Macro (name, True, Just (RangeByOffsetAndCols
                                    { offset = cast offset - 1
                                    , after  = cast line
                                    , startCol  = cast startCol - 1
                                    , endCol    = cast endCol
                                    })))
              Nothing
              (Just src) (Just md) Nothing
      _ => putStrLn katlaCmd.usage
      ]
    ]
  , "--help"  ::= [const $ putStrLn katlaCmd.usage]
  , "preamble" ::= [preamble]
  , "init"     ::= [init]
  ]
