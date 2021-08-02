module Katla.Engine

import System.File
import Core.FC
import Core.Name
import Core.Core
import Core.Metadata
import Libraries.Data.PosMap
import Data.List1
import Data.List
import Data.String
import Data.SnocList

import Katla.Config

escapeLatex : Char -> List Char
escapeLatex '\\' = fastUnpack "\\textbackslash{}"
escapeLatex '{'  = fastUnpack "\\{"
escapeLatex '}'  = fastUnpack "\\}"
escapeLatex x    = [x]

annotate : Maybe Decoration -> String -> String
annotate Nothing    s = s
annotate (Just dec) s = apply (convert dec) s
  where
    convert : Decoration -> String
    convert (Typ     ) = "IdrisType"
    convert (Function) = "IdrisFunction"
    convert (Data    ) = "IdrisData"
    convert (Keyword ) = "IdrisKeyword"
    convert (Bound   ) = "IdrisBound"

    apply : String -> String -> String
    apply f a = "\\\{f}{\{a}}"

color : String -> String
color x = "\\color{\{x}}"

{- Relies on the fact that PosMap is an efficient mapping from position:

for each character in the file, find the tightest enclosing interval
in PosMap and use its decoration.
-}

pickSmallest : List1 ASemanticDecoration -> Decoration
pickSmallest ((_, decor, _) ::: []) = decor
pickSmallest (current ::: candidate :: ds) =
  let endOf : ASemanticDecoration -> (Int, Int)
      endOf ((_, (_, end)), _, _) = end
  in if (endOf candidate < endOf current)
     then pickSmallest (candidate ::: ds)
     else pickSmallest (current   ::: ds)

findDecoration : (Int, Int) -> PosMap ASemanticDecoration -> Maybe Decoration
findDecoration pos@(row, col) posMap =
  case dominators ((row, col), (row, col+1)) posMap of
   []              => Nothing
   (d :: ds)       => Just $ pickSmallest (d ::: ds)

export
engine : (input, output : File)
       -> PosMap ASemanticDecoration
       -> (Int, Int)
       -> IO ()
engine input output posMap = engine Nothing
  where
    toString : SnocList Char -> String
    toString sx = (fastPack $ sx <>> [])

    snocEscape : (outputChars : SnocList Char) -> (new : Char) -> SnocList Char
    snocEscape sx c = sx <>< (escapeLatex c)

    ||| True if input starts with EOL
    isNotEndOfLine : List Char -> Maybe (Char, List Char)
    isNotEndOfLine []           = Nothing
    isNotEndOfLine ('\r' :: _ ) = Nothing
    isNotEndOfLine ('\n' :: _ ) = Nothing
    isNotEndOfLine (x    :: xs) = Just (x, xs)

    ship : Maybe Decoration -> (outputChars : SnocList Char) -> IO ()
    ship decor outputChars = when (isSnoc outputChars) $ do
      let decorated = annotate decor (toString outputChars)
      _ <- fPutStr output decorated
      pure ()

    processLine : (currentDecor  : Maybe Decoration)
               -> (currentPos    : (Int, Int))
               -> (remainingLine : List Char)
               -> (currentOutput : SnocList Char)
               -> IO (Maybe Decoration, (Int, Int))
    processLine currentDecor currentPos@(currentRow, currentCol) cs currentOutput
      = case isNotEndOfLine cs of
          Nothing => do
            let nextPos = (currentRow + 1, 0)
            ship currentDecor currentOutput
            _ <- fPutStrLn output ""
            pure (currentDecor, nextPos)
          Just (c , rest) => do
            let (currentRow, currentCol) = currentPos
                nextPos = (currentRow, currentCol + 1)
                decor   = findDecoration currentPos posMap
            if decor == currentDecor
             then processLine currentDecor nextPos rest (snocEscape currentOutput c)
             else do ship currentDecor currentOutput
                     processLine decor nextPos rest (snocEscape [<] c)

    engine : Maybe Decoration -> (Int, Int) -> IO ()
    engine currentDecor currentPos
      = when (not !(fEOF input)) $ do
          Right str <- fGetLine input
            | Left err => pure ()
          (nextDecor, nextPos) <- processLine currentDecor currentPos (fastUnpack str) [<]
          engine nextDecor nextPos

standalonePre : Config -> String
standalonePre config = """
  \\documentclass{article}

  \\usepackage{fancyvrb}
  \\usepackage[x11names]{xcolor}

  \{laTeXHeader config}

  \\begin{document}
  %\\VerbatimInput[commandchars=\\\\\\{\\}]{content}
  \\begin{Verbatim}[commandchars=\\\\\\{\\}]
  """

standalonePost : String
standalonePost = """
  \\end{Verbatim}
  \\end{document}
  """

export
katla : (snippet : Bool) -> (mconfig : Maybe String) ->
  (msourcefile, mmetadata, moutput : Maybe String) ->
  -- TODO: would be nice to only specify one of source/metadata
  IO ()
katla _       _       Nothing _       _ = putStrLn "Expecting source file to print."
katla _       _       _       Nothing _ = putStrLn "Expecting metadata file to output."
katla False   mconfig (Just filename) (Just metadata) moutput= do
  config <- getConfiguration mconfig
  Right fin <- openFile filename  Read
  | Left err => putStrLn "Couldn't open source file: \{filename}."
  Just fmd <- coreRun (map Just (readMetadata metadata))
                      (\err => do putStrLn $ show err
                                  pure Nothing)
                      pure
    | Nothing => putStrLn "Couldn't open metadata file: \{metadata}"

  Right fout <- maybe (pure $ Right stdout) (\output => openFile output WriteTruncate) moutput
  | Left err => putStrLn "couldn't open output: "

  Right _ <- fPutStrLn fout (standalonePre config)
  | Left err => putStrLn "Error while generating preamble: \{show err}"
  engine fin fout fmd.semanticHighlighting (0,0)
  Right _ <- fPutStrLn fout standalonePost
  | Left err => putStrLn "Error while generating preamble: \{show err}"
  closeFile fout

katla True mconfig msrc mmeta moutput = putStrLn "Not yet implemented."
