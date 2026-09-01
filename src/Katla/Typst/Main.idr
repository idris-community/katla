module Katla.Typst.Main

import public Katla.CLI

import Katla.Config
import Katla.Typst
import Katla.Engine

%default covering
%hide Collie.Modifiers.infix.(::=)

typstCmd : Command "typst"
typstCmd = MkCommand
  { description = "Typst backend"
  , subcommands =
    [ "--help"   ::= basic "Print this help text." none
    , "preamble" ::= preambleCommand
    , "init"     ::= initTypstCommand
    ]
  , modifiers   = ["--config" ::= option """
                    Preamble configuration file in Dhall format.
                    Use `init` to generate the defaults config file.
                    """ filePath
                  ]
  , arguments = lotsOf filePath
  }

typstExec : Typst.Main.typstCmd ~~> IO ()
typstExec =
  [ \parsed => case parsed.arguments of
       Just [src, md, output] =>
         katla Typst
               Nothing -- (rawSnippet $ parsed.modifiers.project "--snippet")
               (parsed.modifiers.project "--config")
               (Just src) (Just md) (Just output)
       Just [src, md]         =>
         katla Typst
               Nothing -- (rawSnippet $ parsed.modifiers.project "--snippet")
               (parsed.modifiers.project "--config")
               (Just src) (Just md) Nothing
       _ => failWithUsage typstCmd
  , "--help"   ::= [ const (putStrLn typstCmd.usage) ]
  , "preamble" ::= [Typst.preamble]
  , "init"     ::= [Typst.init]
  ]

main : IO ()
main = typstCmd.handleWith typstExec
