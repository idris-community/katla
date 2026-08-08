||| Functions for generating highlighted typst code snippets
module Katla.Typst

import Core.Metadata
import System.File

import Collie
import Katla.Config

%hide Collie.Modifiers.infix.(::=)

export
escapeTypst : Char -> List Char
escapeTypst '\n' = ['\\', '\n']
escapeTypst '\r' = ['\\', '\r']
escapeTypst '\t' = ['\\', '\t']
escapeTypst '\\' = ['\\', '\\']
escapeTypst '"'  = ['\\', '"']
escapeTypst x    = [x]

export
annotate : Maybe Decoration -> String -> String
annotate Nothing    s = "#\"\{s}\""
annotate (Just dec) s = apply (convert dec) s
  where

    apply : String -> String -> String
    apply f a = "#\{f}[#\"\{a}\"]"

export
typstHeader : Config -> String
typstHeader cfg =  """

#let IdrisCodeFont        = "\{cfg.font}"
#let IdrisColourData      = \{cfg.datacons.colour}
#let IdrisColourType      = \{cfg.typecons.colour}
#let IdrisColourBound     = \{cfg.bound.colour}
#let IdrisColourFunction  = \{cfg.function.colour}
#let IdrisColourKeyword   = \{cfg.keyword.colour}
#let IdrisColourImplicit  = \{cfg.bound.colour}
#let IdrisColourComment   = \{cfg.comment.colour}
#let IdrisColourHole      = \{cfg.hole.colour}
#let IdrisColourNamespace = \{cfg.namespce.colour}
#let IdrisColourPostulate = \{cfg.postulte.colour}
#let IdrisColourModule    = \{cfg.aModule.colour}

#let IdrisHighlight(col, styl, wei, cont) = {
  set text(fill: col, style: styl, weight: wei)
  cont
}

#let IdrisHole(cont) = {
  set text(fill: IdrisColourHole\{cfg.hole.style})
  cont
}

#let IdrisCode(cont) = {
  set text(font: IdrisCodeFont, size: 0.8em)
  cont
}

#let IdrisData(txt)      = IdrisHighlight(IdrisColourData\{cfg.datacons.style},txt)
#let IdrisType(txt)      = IdrisHighlight(IdrisColourType\{cfg.typecons.style},txt)
#let IdrisBound(txt)     = IdrisHighlight(IdrisColourBound\{cfg.bound.style},txt)
#let IdrisFunction(txt)  = IdrisHighlight(IdrisColourFunction\{cfg.function.style},txt)
#let IdrisKeyword(txt)   = IdrisHighlight(IdrisColourKeyword\{cfg.keyword.style},txt)
#let IdrisImplicit(txt)  = IdrisHighlight(IdrisColourImplicit\{cfg.bound.style},txt)
#let IdrisComment(txt)   = IdrisHighlight(IdrisColourComment\{cfg.comment.style},txt)
#let IdrisNamespace(txt) = IdrisHighlight(IdrisColourNamespace\{cfg.namespce.style},txt)
#let IdrisPostulate(txt) = IdrisHighlight(IdrisColourPostulate\{cfg.postulte.style},txt)
#let IdrisModule(txt)    = IdrisHighlight(IdrisColourModule\{cfg.aModule.style},txt)
"""


export
standalonePre : Config -> String
standalonePre config = ""

export
makeMacroPre : String -> String
makeMacroPre name = """
#IdrisCode[
"""

export
makeMacroPost : String
makeMacroPost = """
]
"""

export
makeInlineMacroPre : String -> String
makeInlineMacroPre name = ""

export
makeInlineMacroPost : String
makeInlineMacroPost = ""

export
mkDriver : Config -> Driver
mkDriver config = MkDriver
  (\_, _ => "", " \\ ")
  escapeTypst
  annotate
  (standalonePre config, "")
  (makeInlineMacroPre, makeInlineMacroPost)
  (makeMacroPre, makeMacroPost)

preambleExec : (moutput : Maybe String) -> (configFile : Maybe String) -> IO ()
preambleExec moutput configFile = do
  Right file <- maybe (pure $ Right stdout) (flip openFile WriteTruncate) moutput
  | Left err => putStrLn """
              Error while opening preamble file \{maybe "stdout" id moutput}:
              \{show err}
              """
  config <- getConfiguration Typst configFile
  Right () <- fPutStr file $ typstHeader config
  | Left err => putStrLn """
      Error while writing preamble file \{fromMaybe "stdout" moutput}:
      \{show err}
      """
  closeFile file

public export
preambleCommand : Command "preamble"
preambleCommand = MkCommand
  { description = "Generate Typst preamble to be used in `template.typ`"
  , subcommands = []
  , modifiers =
    [ "--config" ::= option """
        Preamble configuration file in Dhall format.
        Use `init` to generate the defaults config file.
        """
        filePath
    ]
  , arguments = filePath
  }

export
preamble : (ParsedCommand _ Typst.preambleCommand) -> IO ()
preamble parsed = preambleExec parsed.arguments (parsed.modifiers.project "--config")

public export
initTypstCommand : Command "init"
initTypstCommand = MkCommand
  { description = "Generate preamble configuration file"
  , subcommands = []
  , modifiers = []
  , arguments = filePath
  }

export
init : (ParsedCommand _ Typst.initTypstCommand) -> IO ()
init parsed = initExec Typst parsed.arguments
