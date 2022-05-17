module Katla.Config

import Idrall.API.V2
import Language.Reflection
import Collie
import System.File
import System.Path
import Data.Maybe
import Core.Metadata

%language ElabReflection

export
convert : Decoration -> String
convert Typ        = "IdrisType"
convert Function   = "IdrisFunction"
convert Data       = "IdrisData"
convert Keyword    = "IdrisKeyword"
convert Bound      = "IdrisBound"
convert Namespace  = "IdrisNamespace"
convert Postulate  = "IdrisPostulate"
convert Module     = "IdrisModule"
convert Comment    = "IdrisComment"

public export
record Category where
  constructor MkCategory
  style : String
  colour : String

public export
record Config where
  constructor MkConfig
  font, space : String
  datacons : Category
  typecons : Category
  bound    : Category
  function : Category
  keyword  : Category
  comment  : Category
  hole     : Category
  namespce : Category
  postulte : Category
  aModule  : Category

namespace Cat
  export
  (.toString) : Category -> (prefixString : String) ->
                {default (length prefixString) prefixLength : Nat} -> String
  cat.toString prefixString =
    let initPadding = (prefixLength `minus` (length prefixString `minus` 2))
        restPadding = prefixLength + 2
    in """
    \{prefixString}\{replicate initPadding ' '  } = { style  = \{show cat.style }
    \{indent restPadding "   "}, colour = \{show cat.colour}}
    """

export
(.toString) : Config -> String
conf.toString = """
{ font  = \{show conf.font}
, space = \{show conf.space}
\{conf.datacons.toString ", datacons" {prefixLength = length "datacons"}}
\{conf.typecons.toString ", typecons" {prefixLength = length "datacons"}}
\{conf.bound   .toString ", bound"    {prefixLength = length "datacons"}}
\{conf.function.toString ", function" {prefixLength = length "datacons"}}
\{conf.keyword .toString ", keyword"  {prefixLength = length "datacons"}}
\{conf.comment .toString ", comment"  {prefixLength = length "datacons"}}
\{conf.hole    .toString ", hole"     {prefixLength = length "datacons"}}
\{conf.namespce.toString ", namespce" {prefixLength = length "datacons"}}
\{conf.postulte.toString ", postulte" {prefixLength = length "datacons"}}
\{conf.aModule .toString ", aModule"  {prefixLength = length "datacons"}}
}
"""


export
defaultHTMLConfig : Config
defaultHTMLConfig = MkConfig
  { font = #"\ttfamily"#
  , space = "&nbsp;"
  , datacons = MkCategory
    { style  = ""
    , colour = "darkred"
    }
  , typecons = MkCategory
    { style  = ""
    , colour = "blue"
    }
  , bound = MkCategory
    { style  = ""
    , colour = "black"
    }
  , function = MkCategory
    { style  = ""
    , colour = "darkgreen"
    }
  , keyword = MkCategory
    { style  = "text-decoration: underline;"
    , colour = ""
    }
  , comment = MkCategory
    { style  = ""
    , colour = "#b22222"
    }
  , hole = MkCategory
    { style  = "font-weight: bold;"
    , colour = "yellow"
    }
  , namespce = MkCategory
    { style = "font-style: italic;"
    , colour = "black"
    }
  , postulte = MkCategory
    { style = "font-weight: bold;"
    , colour = "red"
    }
  , aModule  = MkCategory
    { style = "font-style: italic;"
    , colour = "black"
    }
  }

export
defaultLatexConfig : Config
defaultLatexConfig = MkConfig
  { font = #"\ttfamily"#
  , space = #" "#
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
  , namespce = MkCategory
    { style = #"\itshape"#
    , colour = "black"
    }
  , postulte = MkCategory
    { style = #"\bfseries"#
    , colour = "DarkOrchid3"
    }
  , aModule  = MkCategory
    { style = #"\itshape"#
    , colour = "black"
    }
  }
%runElab (deriveFromDhall Record `{ Category })
%runElab (deriveFromDhall Record `{ Config })

public export
data Backend = LaTeX | HTML | Markdown

export
defaultConfig : Backend -> Config
defaultConfig LaTeX = defaultLatexConfig
defaultConfig HTML = defaultHTMLConfig
defaultConfig Markdown = defaultHTMLConfig

export
getConfiguration : Backend -> (configFile : Maybe String) -> IO Config
getConfiguration backend Nothing = pure $ defaultConfig backend
getConfiguration backend (Just filename) = do
  Right config <- liftIOEither (deriveFromDhallString {ty = Config} filename)
  | Left err => do putStrLn  """
                     Error while parsing configuration file \{filename}:
                     \{show err}
                     Using default configuration instead.
                     """
                   pure $ defaultConfig backend

  pure config


public export
record Driver where
  constructor MkDriver
  line        : ((width, lineNumber : Nat) -> String, String)
  escape      : Char -> List Char
  annotate    : Maybe Decoration -> String -> String
  standalone  : (String, String)
  inlineMacro : (String -> String, String)
  blockMacro  : (String -> String, String)

export
initExec : (backend : Backend) -> (moutput : Maybe String) -> IO ()
initExec backend moutput = do
  Right file <- maybe (pure $ Right stdout) (flip openFile WriteTruncate) moutput
  | Left err => do putStrLn """
                            Error while opening configuration file \{fromMaybe "stdout" moutput}:
                            \{show err}
                            """
                   exitFailure
  Right () <- fPutStrLn file $ (defaultConfig backend).toString
  | Left err => do putStrLn """
                            Error while writing preamble file \{fromMaybe "stdout" moutput}:
                            \{show err}
                            """
                   exitFailure
  closeFile file
