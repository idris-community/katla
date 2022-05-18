||| LaTeX rendering of fenced idris codeblocks in literate tex files
module Katla.Literate

import Core.Metadata
import System.File
import System.Directory

import Collie
import Katla.Config
import Katla.LaTeX

import Libraries.Text.PrettyPrint.Prettyprinter.Render.HTML as Lib

export
initStyFile : IO ()
initStyFile = do
  pure ()

export
makeMacroPre : String -> String
makeMacroPre name = """
  \\begin{Verbatim}[commandchars=\\\\\\{\\}]
  """

export
makeMacroPost : String
makeMacroPost = """
  \\end{Verbatim}
  """

export
mkDriver : Config -> Driver
mkDriver config = MkDriver
  (\ wdth, ln => "", "")
  (escapeLatex)
  annotate
  ("", "")
  (const "", "")
  (Literate.makeMacroPre, Literate.makeMacroPost)

public export
initLiterateCmd : Command "init"
initLiterateCmd = MkCommand
  { description = "Generate default configuration file"
  , subcommands = []
  , modifiers = []
  , arguments = filePath
  }

export
init : (ParsedCommand _ Literate.initLiterateCmd) -> IO ()
init parsed = initExec Literate parsed.arguments

export
initSty : Config -> IO ()
initSty cfg = do
  let styFile = "idris2.sty"
  unless !(exists styFile) $ do
    let content = """
                  \\usepackage{inconsolata}
                  \{laTeXHeader cfg}
                  """
    Right () <- writeFile styFile content
      | Left err => do putStrLn "Error: \{show err}"
                       exitFailure
    pure ()
