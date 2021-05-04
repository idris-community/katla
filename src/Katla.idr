module Katla

import System.File
import Core.FC
import Core.Name
import Core.Core
import Core.Metadata
import Libraries.Data.PosMap
import Data.List1
import Data.List
import Data.String

public export
Eq Decoration where
  Typ      == Typ      = True
  Function == Function = True
  Data     == Data     = True
  Keyword  == Keyword  = True
  Bound    == Bound    = True
  _        == _        = False

escapeLatex' : Char -> List Char
escapeLatex' '\\' = unpack "\\textbackslash{}"
escapeLatex' '{'  = unpack "\\{"
escapeLatex' '}'  = unpack "\\}"
escapeLatex' x    = [x]

escapeLatex : Char -> String
escapeLatex '\\' = "\\textbackslash{}"
escapeLatex '{'  = "\\{"
escapeLatex '}'  = "\\}"
escapeLatex x    = cast x
      
decoration : Maybe Decoration -> String
decoration Nothing         = "black"
decoration (Just Typ     ) = "DeepSkyBlue3"
decoration (Just Function) = "Chartreuse4"
decoration (Just Data    ) = "IndianRed1"
decoration (Just Keyword ) = "Azure4"
decoration (Just Bound   ) = "DarkOrchid3"


color : String -> String
color x = "\\color{\{x}}" -- String interpolation doesn't quite work yet

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

engine : (input, output : File) -> PosMap ASemanticDecoration -> (Int, Int) -> IO ()
engine input output posMap = engine Nothing
  where
    processLine : Maybe Decoration -> (Int, Int) -> List Char -> IO (Maybe Decoration, (Int, Int))
    processLine currentDecor (currentRow, _) [] = do
      let nextPos = (currentRow + 1, 0)
      pure (currentDecor, nextPos)
    processLine currentDecor currentPos@(currentRow, currentCol) (c :: rest) = 
      let nextPos = (currentRow, currentCol + 1) 
          decor   = findDecoration currentPos posMap in
      if decor == currentDecor 
      then do _ <- fPutStr output (escapeLatex c)
              processLine currentDecor nextPos rest
      else do _ <- fPutStr output $ color (decoration decor) ++ escapeLatex c
              processLine decor        nextPos rest
  
    engine : Maybe Decoration -> (Int, Int) -> IO ()
    engine currentDecor currentPos 
      = if !(fEOF input)
      then pure ()
      else do
        Right str <- fGetLine input
          | Left err => pure ()
        (nextDecor, nextPos) <- processLine currentDecor currentPos (fastUnpack str)
        engine nextDecor nextPos
    
main : IO ()
main = do
  putStrLn "Katla v0.1"
  Right fin <- openFile "src/Katla.idr"  Read
    | Left err => putStrLn "Couldn't open source."
  Just fmd <- coreRun (map Just (readMetadata "build/ttc/Katla.ttm"))
                      (const $ pure Nothing)
                      pure
    | Nothing => putStrLn "Couldn't open metadata"
  
  Right fout <- openFile "temp/Katla.tex" WriteTruncate
    | Left err => putStrLn "couldn't open output"

  {- Prints annotations, for debugging purposes -}
  -- let decs = foldr (\x,xs => Prelude.(::) x xs) Prelude.Nil $ fmd.semanticHighlighting
  -- traverse_ (\((_, (start, end)), decor, _) => putStrLn "\{show start}:\{show end}: \{show decor}")
  --           decs
  engine fin fout fmd.semanticHighlighting (0,0)
