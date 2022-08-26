||| HTML rendering of fenced idris codeblocks in markdown files
module Katla.Markdown

import Core.Metadata
import System.File

import Collie
import Katla.Config
import Katla.HTML

import Libraries.Text.PrettyPrint.Prettyprinter.Render.HTML as Lib

export
escapeMarkdown : Config -> Char -> List Char
escapeMarkdown config ' ' = unpack config.space
escapeMarkdown config '_' = unpack "\\_"
escapeMarkdown config '*' = unpack "\\*"
escapeMarkdown config '$' = unpack "\\$"
escapeMarkdown config c = unpack (htmlEscape $ cast c)

export
standalonePre : Config -> String
standalonePre config = """
  <style>
  \{styleHeader config}
  .IdrisCode {
    display: block;
    background-color: whitesmoke;
  }
  </style>
  """

export
mkDriver : Config -> Driver
mkDriver config = MkDriver
  (\ wdth, ln => "", "<br />")
  (escapeMarkdown config)
  annotate
  (Markdown.standalonePre config, "")
  (makeInlineMacroPre, makeInlineMacroPost)
  (makeMacroPre, makeMacroPost)

public export
initMarkdownCmd : Command "init"
initMarkdownCmd = MkCommand
  { description = "Generate default configuration file"
  , subcommands = []
  , modifiers = []
  , arguments = filePath
  }

export
init : (ParsedCommand _ Markdown.initMarkdownCmd) -> IO ()
init parsed = initExec Markdown parsed.arguments
