||| Functions for generating
module Katla.LaTeX

import Core.Metadata
import System.File

import Collie
import Katla.Config

export
escapeLatex : Char -> List Char
escapeLatex '-' = fastUnpack "\\KatlaDash{}"
escapeLatex '&' = fastUnpack "\\&"
escapeLatex '%' = fastUnpack "\\%"
escapeLatex '\\' = fastUnpack "\\textbackslash{}"
escapeLatex '{'  = fastUnpack "\\{"
escapeLatex '}'  = fastUnpack "\\}"
escapeLatex '$'  = fastUnpack "\\$"
escapeLatex ' '  = fastUnpack "\\KatlaSpace{}"
escapeLatex '_'  = fastUnpack "\\KatlaUnderscore{}"
escapeLatex '~'  = fastUnpack "\\KatlaTilde{}"
escapeLatex x    = [x]

export
annotate : Maybe Decoration -> String -> String
annotate Nothing    s = s
annotate (Just dec) s = apply (convert dec) s
  where

    apply : String -> String -> String
    apply f a = "\\\{f}{\{a}}"

export
laTeXHeader : Config -> String
laTeXHeader cfg =  """
\\usepackage{fancyvrb}
\\usepackage[x11names]{xcolor}
\\newcommand{\\Katla}                [2][]{\\VerbatimInput[commandchars=\\\\\\{\\}#1]{#2}}
\\newcommand{\\KatlaNewline}            {}
\\newcommand{\\KatlaSpace}              {\{cfg.space}}
\\newcommand{\\KatlaDash}               {\\string-}
\\newcommand{\\KatlaUnderscore}         {\\string_}
\\newcommand{\\KatlaTilde}              {\\string~}
\\newcommand{\\IdrisHlightFont}         {\{cfg.font}}
\\newcommand{\\IdrisHlightStyleData}    {\{cfg.datacons .style}}
\\newcommand{\\IdrisHlightStyleType}    {\{cfg.typecons .style}}
\\newcommand{\\IdrisHlightStyleBound}   {\{cfg.bound    .style}}
\\newcommand{\\IdrisHlightStyleFunction}{\{cfg.function .style}}
\\newcommand{\\IdrisHlightStyleKeyword} {\{cfg.keyword  .style}}
\\newcommand{\\IdrisHlightStyleImplicit}{\{cfg.bound    .style}}
\\newcommand{\\IdrisHlightStyleComment} {\{cfg.comment  .style}}
\\newcommand{\\IdrisHlightStyleHole}    {\{cfg.hole     .style}}
\\newcommand{\\IdrisHlightStyleNamespace}{\{cfg.namespce.style}}
\\newcommand{\\IdrisHlightStylePostulate}{\{cfg.postulte.style}}
\\newcommand{\\IdrisHlightStyleModule}   {\{cfg.aModule .style}}

\\newcommand{\\IdrisHlightColourData}    {\{cfg.datacons .colour}}
\\newcommand{\\IdrisHlightColourType}    {\{cfg.typecons .colour}}
\\newcommand{\\IdrisHlightColourBound}   {\{cfg.bound    .colour}}
\\newcommand{\\IdrisHlightColourFunction}{\{cfg.function .colour}}
\\newcommand{\\IdrisHlightColourKeyword} {\{cfg.keyword  .colour}}
\\newcommand{\\IdrisHlightColourImplicit}{\{cfg.bound    .colour}}
\\newcommand{\\IdrisHlightColourComment} {\{cfg.comment  .colour}}
\\newcommand{\\IdrisHlightColourHole}    {\{cfg.hole     .colour}}
\\newcommand{\\IdrisHlightColourNamespace}{\{cfg.namespce.colour}}
\\newcommand{\\IdrisHlightColourPostulate}{\{cfg.postulte.colour}}
\\newcommand{\\IdrisHlightColourModule}   {\{cfg.aModule .colour}}

\\newcommand{\\IdrisHole}[1]{{%
    \\colorbox{\\IdrisHlightColourHole}{%
      \\IdrisHlightStyleHole\\IdrisHlightFont%
      #1}}}

\\newcommand{\\RawIdrisHighlight}[3]{{\\textcolor{#1}{\\IdrisHlightFont#2{#3}}}}

\\newcommand{\\IdrisData}[1]{\\RawIdrisHighlight{\\IdrisHlightColourData}{\\IdrisHlightStyleData}{#1}}
\\newcommand{\\IdrisType}[1]{\\RawIdrisHighlight{\\IdrisHlightColourType}{\\IdrisHlightStyleType}{#1}}
\\newcommand{\\IdrisBound}[1]{\\RawIdrisHighlight{\\IdrisHlightColourBound}{\\IdrisHlightStyleBound}{#1}}
\\newcommand{\\IdrisFunction}[1]{\\RawIdrisHighlight{\\IdrisHlightColourFunction}{\\IdrisHlightStyleFunction}{#1}}
\\newcommand{\\IdrisKeyword}[1]{\\RawIdrisHighlight{\\IdrisHlightColourKeyword}{\\IdrisHlightStyleKeyword}{#1}}
\\newcommand{\\IdrisImplicit}[1]{\\RawIdrisHighlight{\\IdrisHlightColourImplicit}{\\IdrisHlightStyleImplicit}{#1}}
\\newcommand{\\IdrisComment}[1]{\\RawIdrisHighlight{\\IdrisHlightColourComment}{\\IdrisHlightStyleComment}{#1}}
\\newcommand{\\IdrisNamespace}[1]{\\RawIdrisHighlight{\\IdrisHlightColourNamespace}{\\IdrisHlightStyleNamespace}{#1}}
\\newcommand{\\IdrisPostulate}[1]{\\RawIdrisHighlight{\\IdrisHlightColourPostulate}{\\IdrisHlightStylePostulate}{#1}}
\\newcommand{\\IdrisModule}[1]{\\RawIdrisHighlight{\\IdrisHlightColourModule}{\\IdrisHlightStyleModule}{#1}}

\\newenvironment{code}
  {\\begin{Verbatim}[commandchars=\\\\\\{\\}]}
  {\\end{Verbartim}}

% Bugfix in fancyvrb to allow inline saved listings
\\makeatletter
\\let\\FV@ProcessLine\\relax
\\makeatother

"""


export
standalonePre : Config -> String
standalonePre config = """
  \\documentclass{article}

  \\usepackage{inconsolata}

  \{laTeXHeader config}

  \\begin{document}
  \\begin{Verbatim}[commandchars=\\\\\\{\\}]
  """

export
standalonePost : String
standalonePost = """
  \\end{Verbatim}
  \\end{document}
  """

export
makeMacroPre : String -> String
makeMacroPre name = """
  \\newcommand\\\{name}[1][]{\\UseVerbatim[#1]{\{name}}}
  \\begin{SaveVerbatim}[commandchars=\\\\\\{\\}]{\{name}}
  """

export
makeMacroPost : String
makeMacroPost = """
  \\end{SaveVerbatim}
  """

export
makeInlineMacroPre : String -> String
makeInlineMacroPre name = """
  \\newcommand\\\{name}[1][]{\\UseVerb[#1]{\{name}}}
  \\begin{SaveVerbatim}[commandchars=\\\\\\{\\}]{\{name}}
  """

export
makeInlineMacroPost : String
makeInlineMacroPost = """
  \\end{SaveVerbatim}
  """

export
mkDriver : Config -> Driver
mkDriver config = MkDriver
  (\_, _ => "", "\\KatlaNewline{}")
  escapeLatex
  annotate
  (standalonePre config, standalonePost)
  (makeInlineMacroPre, makeInlineMacroPost)
  (makeMacroPre, makeMacroPost)

public export
preambleCmd : Command "preamble"
preambleCmd = MkCommand
  { description = "Generate LaTeX preamble"
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

public export
initLatexCmd : Command "init"
initLatexCmd = MkCommand
  { description = "Generate preamble configuration file"
  , subcommands = []
  , modifiers = []
  , arguments = filePath
  }


preambleExec : (moutput : Maybe String) -> (configFile : Maybe String) -> IO ()
preambleExec moutput configFile = do
  Right file <- maybe (pure $ Right stdout) (flip openFile WriteTruncate) moutput
  | Left err => putStrLn """
              Error while opening preamble file \{maybe "stdout" id moutput}:
              \{show err}
              """
  config <- getConfiguration LaTeX configFile
  Right () <- fPutStr file $ laTeXHeader config
  | Left err => putStrLn """
      Error while writing preamble file \{fromMaybe "stdout" moutput}:
      \{show err}
      """
  closeFile file

export
preamble : (ParsedCommand _ LaTeX.preambleCmd) -> IO ()
preamble parsed = preambleExec parsed.arguments (parsed.modifiers.project "--config")

export
init : (ParsedCommand _ LaTeX.initLatexCmd) -> IO ()
init parsed = initExec LaTeX parsed.arguments
