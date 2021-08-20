module Katla.Config

import Idrall.API.V2
import Language.Reflection
import Collie
import System.File
import System.Path
import Data.Maybe

%language ElabReflection

public export
record Category where
  constructor MkCategory
  style : String
  colour : String

public export
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
    \{indent restPadding "   "}, colour = \{show cat.colour}}
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


export
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
