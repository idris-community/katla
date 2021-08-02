module Katla.CLI

import public Collie

import Katla.Config
import Katla.Engine

%default covering

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

export
katlaExec : CLI.katlaCmd ~~> IO ()
katlaExec =
  [ \parsed => case parsed.arguments of
       Just [src, md, output] => katla (parsed.modifiers.project "--snippet")
                                       (parsed.modifiers.project "--config")
                                       (Just src) (Just md) (Just output)
       Just [src, md]         => katla (parsed.modifiers.project "--snippet")
                                       (parsed.modifiers.project "--config")
                                       (Just src) (Just md) Nothing
       _ => putStrLn katlaCmd.usage
  ,  "--help"  ::= [const $ putStrLn katlaCmd.usage]
  , "preamble" ::= [preamble]
  , "init"     ::= [init]
  ]
