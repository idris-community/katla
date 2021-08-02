module Katla.Config

import Idrall.API.V2
import Language.Reflection
import Collie
import System.File
import System.Path
import Data.Maybe

%language ElabReflection

record Category where
  constructor MkCategory
  style : String
  colour : String

export
record Config where
  constructor MkConfig
  font : String
  datacons : Category
  typecons : Category
  bound    : Category
  function : Category
  keyword  : Category
  comment  : Category
  hole     : Category

namespace Cat
  export
  (.toString) : Category -> (prefixString : String) ->
                {default (length prefixString) prefixLength : Nat} -> String
  cat.toString prefixString =
    let initPadding = (prefixLength `minus` (length prefixString `minus` 2))
        restPadding = prefixLength + 2
    in """
    \{prefixString}\{replicate initPadding ' '  } = { style  = \{show cat.style }
    \{indent restPadding " = "}, colour = \{show cat.colour}}
    """

export
(.toString) : Config -> String
conf.toString = """
{ font = \{show conf.font}
\{conf.datacons.toString ", datacons" {prefixLength = length "datacons"}}
\{conf.typecons.toString ", typecons" {prefixLength = length "datacons"}}
\{conf.bound   .toString ", bound"    {prefixLength = length "datacons"}}
\{conf.function.toString ", function" {prefixLength = length "datacons"}}
\{conf.keyword .toString ", keyword"  {prefixLength = length "datacons"}}
\{conf.comment .toString ", comment"  {prefixLength = length "datacons"}}
\{conf.hole    .toString ", hole"     {prefixLength = length "datacons"}}
}
"""


defaultConfig : Config
defaultConfig = MkConfig
  { font = #"\ttfamily"#
  , datacons = MkCategory
    { style  = ""
    , colour = "IndianRed1"
    }
  , typecons = MkCategory
    { style  = ""
    , colour = "DeepSkyBlue3"
    }
  , bound = MkCategory
    { style  = ""
    , colour = "DarkOrchid3"
    }
  , function = MkCategory
    { style  = ""
    , colour = "Chartreuse4"
    }
  , keyword = MkCategory
    { style  = #"\bfseries"#
    , colour = "black"
    }
  , comment = MkCategory
    { style  = #"\itshape"#
    , colour = "grey"
    }
  , hole = MkCategory
    { style  = #"\bfseries"#
    , colour = "yellow"
    }
  }
%runElab (deriveFromDhall Record `{ Category })
%runElab (deriveFromDhall Record `{ Config })

||| Not yet used
export
laTeXHeader : Config -> String
laTeXHeader cfg =  """
\\newcommand{\\IdrisHlightFont}         {\{cfg.font}}
\\newcommand{\\IdrisHlightStyleData}    {\{cfg.datacons.style}}
\\newcommand{\\IdrisHlightStyleType}    {\{cfg.typecons.style}}
\\newcommand{\\IdrisHlightStyleBound}   {\{cfg.bound   .style}}
\\newcommand{\\IdrisHlightStyleFunction}{\{cfg.function.style}}
\\newcommand{\\IdrisHlightStyleKeyword} {\{cfg.keyword .style}}
\\newcommand{\\IdrisHlightStyleImplicit}{\{cfg.bound   .style}}
\\newcommand{\\IdrisHlightStyleComment} {\{cfg.comment .style}}
\\newcommand{\\IdrisHlightStyleHole}    {\{cfg.hole    .style}}

\\newcommand{\\IdrisHlightColourData}    {\{cfg.datacons.colour}}
\\newcommand{\\IdrisHlightColourType}    {\{cfg.typecons.colour}}
\\newcommand{\\IdrisHlightColourBound}   {\{cfg.bound   .colour}}
\\newcommand{\\IdrisHlightColourFunction}{\{cfg.function.colour}}
\\newcommand{\\IdrisHlightColourKeyword} {\{cfg.keyword .colour}}
\\newcommand{\\IdrisHlightColourImplicit}{\{cfg.bound   .colour}}
\\newcommand{\\IdrisHlightColourComment} {\{cfg.comment .colour}}
\\newcommand{\\IdrisHlightColourHole}    {\{cfg.hole    .colour}}

\\newcommand{\\IdrisHole}[1]{{%
    \\colorbox{yellow}{%
      \\IdrisHlightStyleHole\\IdrisHlightFont%
      #1}}}

\\newcommand{\\RawIdrisHighlight}[3]{{\\textcolor{#1}{#2\\IdrisHlightFont#3}}}

\\newcommand{\\IdrisData}[1]{\\RawIdrisHighlight{\\IdrisHlightColourData}{\\IdrisHlightStyleData}{#1}}
\\newcommand{\\IdrisType}[1]{\\RawIdrisHighlight{\\IdrisHlightColourType}{\\IdrisHlightStyleType}{#1}}
\\newcommand{\\IdrisBound}[1]{\\RawIdrisHighlight{\\IdrisHlightColourBound}{\\IdrisHlightStyleBound}{#1}}
\\newcommand{\\IdrisFunction}[1]{\\RawIdrisHighlight{\\IdrisHlightColourFunction}{\\IdrisHlightStyleFunction}{#1}}
\\newcommand{\\IdrisKeyword}[1]{\\RawIdrisHighlight{\\IdrisHlightColourKeyword}{\\IdrisHlightStyleKeyword}{#1}}
\\newcommand{\\IdrisImplicit}[1]{\\RawIdrisHighlight{\\IdrisHlightColourImplicit}{\\IdrisHlightStyleImplicit}{#1}}
\\newcommand{\\IdrisComment}[1]{\\RawIdrisHighlight{\\IdrisHlightColourComment}{\\IdrisHlightStyleComment}{#1}}
"""

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
initCmd : Command "init"
initCmd = MkCommand
  { description = "Generate preamble configuration file"
  , subcommands = []
  , modifiers = []
  , arguments = filePath
  }



export
getConfiguration : (configFile : Maybe String) -> IO Config
getConfiguration Nothing = pure defaultConfig
getConfiguration (Just filename) = do
  Right config <- liftIOEither (deriveFromDhallString {ty = Config} filename)
  | Left err => do putStrLn  """
                     Error while parsing configuration file \{filename}:
                     \{show err}
                     Using default configuration instead.
                     """
                   pure defaultConfig

  pure config

preambleExec : (moutput : Maybe String) -> (configFile : Maybe String) -> IO ()
preambleExec moutput configFile = do
  Right file <- maybe (pure $ Right stdout) (flip openFile WriteTruncate) moutput
  | Left err => putStrLn """
              Error while opening preamble file \{maybe "stdout" id moutput}:
              \{show err}
              """
  config <- getConfiguration configFile
  Right () <- fPutStr file $ laTeXHeader config
  | Left err => putStrLn """
      Error while writing preamble file \{maybe "stdout" id moutput}:
      \{show err}
      """
  closeFile file

export
preamble : (ParsedCommand Prelude.id Maybe _ Config.preambleCmd) -> IO ()
preamble parsed = preambleExec parsed.arguments (parsed.modifiers.project "--config")

export
initExec : (moutput : Maybe String) -> IO ()
initExec moutput = do
  Right file <- maybe (pure $ Right stdout) (flip openFile WriteTruncate) moutput
  | Left err => putStrLn """
              Error while opening configuration file \{maybe "stdout" id moutput}:
              \{show err}
              """
  Right () <- fPutStrLn file $ defaultConfig.toString
  | Left err => putStrLn """
      Error while writing preamble file \{maybe "stdout" id moutput}:
      \{show err}
      """
  closeFile file

export
init : (ParsedCommand Prelude.id Maybe _ Config.initCmd) -> IO ()
init parsed = initExec parsed.arguments
