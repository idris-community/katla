||| Functions for generating
module Katla.HTML

import Core.Metadata
import System.File

import Collie
import Katla.Config

import Libraries.Text.PrettyPrint.Prettyprinter.Render.HTML as Lib

export
escapeHTML : Config -> Char -> List Char
escapeHTML config ' ' = unpack config.space
escapeHTML config c = unpack (htmlEscape $ cast c)

export
annotate : Maybe Decoration -> String -> String
annotate Nothing    s = s
annotate (Just dec) s = apply (convert dec) s
  where
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

    apply : String -> String -> String
    apply f a = #"<span class="\#{f}">\#{a}</span>"#

export
styleHeader : Config -> String
styleHeader cfg =  """
    .IdrisData {
      \{cfg.datacons .style}
      color: \{cfg.datacons .colour}
    }
    .IdrisType {
      \{cfg.typecons .style}
      color: \{cfg.typecons .colour}
    }
    .IdrisBound {
      \{cfg.bound .style}
      color: \{cfg.bound .colour}
    }
    .IdrisFunction {
      \{cfg.function .style}
      color: \{cfg.function .colour}
    }
    .IdrisKeyword {
      \{cfg.keyword .style}
      color: \{cfg.keyword .colour}
    }
    .IdrisImplicit {
      \{cfg.bound .style}
      color: \{cfg.bound .colour}
    }
    .IdrisComment {
      \{cfg.comment .style}
      color: \{cfg.comment .colour}
    }
    .IdrisNamespace {
      \{cfg.namespce .style}
      color: \{cfg.namespce .colour}
    }
    .IdrisPostulate {
      \{cfg.postulte .style}
      color: \{cfg.postulte .colour}
    }
    .IdrisModule {
      \{cfg.aModule .style}
      color: \{cfg.aModule .colour}
    }
"""

export
standalonePre : Config -> String
standalonePre config = """
  <!DOCTYPE html><html lang="en">

  <head>
    <meta charset="utf-8">
    <style>
    \{styleHeader config}

    .IdrisLineNumber {
      text-decoration: none;
      color: lightgrey;
      user-select: none;
    }
    .IdrisLineNumber:hover {
      color: darkgray;
    }
    .IdrisLineNumber:target {
      color: gray;
    }
    .IdrisHighlight {
      background-color: yellow;
    }
    </style>
    <script>
    function handleHash() {
      if (!location.hash) return
      let m = location.hash.match(/#(line\\d+)(?:-(line\\d+))?/)
      if (m) {
        let start = document.getElementById(m[1])
        let end = document.getElementById(m[2])
        if (start) {
          start.scrollIntoView()
          let parent = start.parentElement
          let lines = parent.children
          let className = ''
          for (let n = 0; n < lines.length; n++) {
            let el = lines[n]
            if (el === start) className = 'IdrisHighlight'
            el.className = className
            if (el === end || className && !end) className = ''
          }
        }
      }
    }
    window.addEventListener('hashchange',handleHash)
    </script>
  </head>
  <body onload="handleHash()">
  <code>
  """

export
standalonePost : String
standalonePost = """
  </code>
  </body>
  </html>
  """

export
makeMacroPre : String -> String
makeMacroPre name = """
  <code>
  """

export
makeMacroPost : String
makeMacroPost = """
  </code>
  """

export
makeInlineMacroPre : String -> String
makeInlineMacroPre name = """
  <code>
  """

export
makeInlineMacroPost : String
makeInlineMacroPost = """
  </code>
  """

export
mkDriver : Config -> Driver
mkDriver config = MkDriver
  (\ wdth, ln =>
    let ln = show ln
        lineID = "line\{ln}"
        desc = concat (replicate (minus wdth (length ln)) "&nbsp;" ++ [ln]) in
    ##"<div id="\##{lineID}"><a href="#\##{lineID}" class="IdrisLineNumber"> \##{desc} | </a>"##
  , "</div>")
  (escapeHTML config)
  annotate
  (standalonePre config, standalonePost)
  (makeInlineMacroPre, makeInlineMacroPost)
  (makeMacroPre, makeMacroPost)

public export
initHTMLCmd : Command "init"
initHTMLCmd = MkCommand
  { description = "Generate default configuration file"
  , subcommands = []
  , modifiers = []
  , arguments = filePath
  }


export
initExec : (moutput : Maybe String) -> IO ()
initExec moutput = do
  Right file <- maybe (pure $ Right stdout) (flip openFile WriteTruncate) moutput
  | Left err => putStrLn """
              Error while opening configuration file \{maybe "stdout" id moutput}:
              \{show err}
              """
  Right () <- fPutStrLn file $ defaultHTMLConfig.toString
  | Left err => putStrLn """
      Error while writing preamble file \{maybe "stdout" id moutput}:
      \{show err}
      """
  closeFile file

export
init : (ParsedCommand _ HTML.initHTMLCmd) -> IO ()
init parsed = initExec parsed.arguments
