module Katla

import System.File
import Core.FC
import Core.Name
import Core.Core
import Core.Metadata
import Libraries.Data.PosMap
import Data.List.Lazy
import Data.List1

import Numeric


data Event 
  = Plain     (List1 String)
  | Decorated (List1 String) Decoration

escapeLatex : Char -> String
escapeLatex '\\' = "\\backslash"
escapeLatex '{'  = "\\{"
escapeLatex '}'  = "\\}"
escapeLatex x    = cast x

infixr 4 |>

      
decoration : Decoration -> String
decoration Typ      = "DeepSkyBlue3"
decoration Function = "Chartreuse4"
decoration Data     = "IndianRed1"
decoration Keyword  = "Azure4"
decoration Bound    = "DarkOrchid3"

plain : String
plain = "black"

-- x11names
decorate : Decoration -> List1 String -> List1 String
decorate dec (head ::: tail) = (decoration dec ++ head) ::: tail

process : Event -> List1 String
process (Plain (str ::: rest)) = (plain ++ str) ::: rest
process (Decorated strs dec  ) = decorate dec strs


getRemainingLines : File -> IO (List1 String)
getRemainingLines x = ?getRemainingLines_rhs

-- Assumes we indeed have input-many characters/lines!
getLinesCols : File -> (Int, Int) -> IO (List1 String)
getLinesCols file amount
= pure $ reverse !(getLinesCols [] amount) 
  where
    getLinesCols : (acc : List String) -> (Int, Int) -> IO (List1 String)  
    getLinesCols acc (lineCount, colCount)
    = if lineCount <= 0
      then either
             (const $ pure $ "" ::: acc)
             (pure . (::: acc))
           !(fGetChars file colCount)
      else either
             (const $ pure $ "" ::: [])
             (\line => getLinesCols (line :: acc) ?hole001010110{-(lineCount - 1, colCount)-})
           !(fGetLine file)
  
engine : (input, output : File) -> (Int, Int) -> LazyList ASemanticDecoration -> IO ()
engine inFile outFile _ []  
   = do lines <- getRemainingLines inFile
        let output = process (Plain lines)
        Prelude.traverse_ (fPutStrLn outFile) output
engine inFile outFile current@(currentLine, currentCol)  (semdecor :: rest) 
  = case semdecor of
      ((_, (start@(startLine, startCol)
           ,  end@(  endLine,   endCol)))
      , decor
      , _) => let (decorLineDelta, decorColDelta) = let _ : Negative Int = deriveNegative
                                                        _ : Negative (Int, Int) = PairwiseNegative
                                                    in start - current
                  splitLineDelta : Int = startLine - currentLine
              in if splitLineDelta <= 0
                 then do let colDelta = startCol - currentCol
                         if colDelta > 0
                          then do rest <- getLinesCols inFile (0, colDelta)
                                  let output = process (Plain rest)
                                  ?hole1029
                          else ?hole19138
                         --let output = process (Plain rest) ?engine_rhs__
                         ?hoel1838181
                 else ?foo


main : IO ()
main = do
  Right fin <- openFile "src/Katla.idr"  Read
    | Left err => putStrLn "Couldn't open source."
  Just fmd <- coreRun (map Just (readMetadata "build/Katla.ttm"))
                      (const $ pure Nothing)
                      pure
    | Nothing => putStrLn "Couldn't open metadata"
  
  Right fout <- openFile "~/temp/Katla.tex" WriteTruncate
    | Left err => putStrLn "couldn't open output"
  
  --?hole
  
  putStrLn "Katla v0.0"
