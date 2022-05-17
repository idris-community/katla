||| Functions for generating
module Katla.HTML

import Core.Metadata
import Data.String
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

    apply : String -> String -> String
    apply f a = #"<span class="\#{f}">\#{a}</span>"#

-- /!\ Do not introduce additional empty lines as that would probably
--     break the markdown backend!
export
styleElement : Decoration -> Category -> String
styleElement dec cat = concat $ intersperse "\n" $ catMaybes
  [ pure ("." ++ convert dec ++ " {")
  , ("  \{cat .style}") <$ guard (cat .style /= "")
  , ("  color: \{cat .colour}") <$ guard (cat .colour /= "")
  , pure "}"
  ]

export
styleHeader : Config -> String
styleHeader cfg =  """
    \{styleElement Data cfg.datacons}
    \{styleElement Typ cfg.typecons}
    \{styleElement Bound cfg.bound}
    \{styleElement Function cfg.function}
    \{styleElement Keyword cfg.keyword}
    \{styleElement Comment cfg.comment}
    \{styleElement Namespace cfg.namespce}
    \{styleElement Postulate cfg.postulte}
    \{styleElement Module cfg.aModule}
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
      function initialize() {
        function handleHash(ev) {
            if (!location.hash) return
            let m = location.hash.match(/#(line\\d+)(?:-(line\\d+))?/)
            if (m) {
                let start = document.getElementById(m[1])
                let end = document.getElementById(m[2])
                if (start) {
                    if (end && end.compareDocumentPosition(start) === 4) {
                        ([start, end] = [end, start])
                    }
                    // Only on page load
                    if (!ev) start.scrollIntoView()
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
        let startLine
        let endLine
        function handlePointerMove(ev) {
            if (startLine) {
                for (let el = document.elementFromPoint(ev.clientX, ev.clientY); el; el = el.parentElement) {
                    if (el.parentElement === startLine.parentElement) {
                        if (endLine !== el) {
                            endLine = el
                            update()
                        }
                        break
                    }
                }
            }
        }
        function update(ev) {
            window.location.hash = startLine === endLine ? startLine.id : startLine.id + '-' + endLine.id
        }
        function handlePointerDown(ev) {
            let target = ev.target
            if (target.className === 'IdrisLineNumber') {
                startLine = endLine = target.parentElement
                window.addEventListener('pointermove', handlePointerMove)
                update()
                ev.preventDefault()
            }
        }
        function handlePointerUp(ev) {
            if (startLine) {
                update()
                window.removeEventListener('pointermove', handlePointerMove)
                startLine = endLine = null
            }
        }
        window.addEventListener('hashchange', handleHash)
        window.addEventListener('pointerdown', handlePointerDown)
        window.addEventListener('pointerup', handlePointerUp)
        handleHash()
    }
    </script>
  </head>
  <body onload="initialize()">
  <code class="IdrisCode">
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
  <code class="IdrisCode">
  """

export
makeMacroPost : String
makeMacroPost = """
  </code>
  """

export
makeInlineMacroPre : String -> String
makeInlineMacroPre name = """
  <code class="IdrisCode">
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
        desc = concat (List.replicate (minus wdth (length ln)) "&nbsp;" ++ [ln]) in
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
init : (ParsedCommand _ HTML.initHTMLCmd) -> IO ()
init parsed = initExec HTML parsed.arguments
