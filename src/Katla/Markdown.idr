||| Functions for generating
module Katla.Markdown

import Core.Metadata
import System.File

import Collie
import Katla.Config
import Katla.HTML

import Libraries.Text.PrettyPrint.Prettyprinter.Render.HTML as Lib

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
  (escapeHTML config)
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
initExec : (moutput : Maybe String) -> IO ()
initExec moutput = do
  Right file <- maybe (pure $ Right stdout) (flip openFile WriteTruncate) moutput
  | Left err => do putStrLn """
                            Error while opening configuration file \{fromMaybe "stdout" moutput}:
                            \{show err}
                            """
                   exitFailure
  Right () <- fPutStrLn file $ defaultHTMLConfig.toString
  | Left err => do putStrLn """
                            Error while writing preamble file \{fromMaybe "stdout" moutput}:
                            \{show err}
                            """
                   exitFailure
  closeFile file

export
init : (ParsedCommand _ Markdown.initMarkdownCmd) -> IO ()
init parsed = Markdown.initExec parsed.arguments
