module Katla

import System.File
import Core.FC
import Core.Name
import Core.Core
import Core.Metadata
import Libraries.Data.PosMap
import Data.List.Lazy
import Data.List1
import Data.String

import Numeric


toList : PosMap a -> List a
--toList = Libraries.Data.PosMap.traverse (\x => [x])

data Event 
  = Plain     (List1 String)
  | Decorated (List1 String) Decoration

escapeLatex : Char -> List Char
escapeLatex '\\' = unpack "\\backslash"
escapeLatex '{'  = unpack "\\{"
escapeLatex '}'  = unpack "\\}"
escapeLatex x    = [x]

escapeString : String -> String
escapeString = fastPack . (concatMap escapeLatex) . fastUnpack

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
getRemainingLines file = getRemainingLines []
  where
    getRemainingLines : List String -> IO (List1 String)
    getRemainingLines acc 
      = if !(fEOF file)
        then pure $ reverse ("" ::: acc)
        else do Right str <- fGetLine file
                  | Left err => pure $ reverse ("" ::: acc)
                getRemainingLines (str :: acc)
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
             (\line => getLinesCols (line :: acc) 
               (lineCount - 1, colCount))
           !(fGetLine file)



putLines : File -> List1 String -> IO ()
putLines file (head ::: []  ) = do Right () <- fPutStr file $ escapeString head
                                     | Left err => pure ()
                                   pure ()
putLines file (head ::: rest) = do Right () <- fPutStrLn file $ escapeString head
                                     | Left err => pure ()
                                   pure ()

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
      , _) => 
               do let ((decorLineDelta, decorColDelta)
                      ,decoratedRange) : ((Int, Int) , (Int, Int))  = 
                       let _ : Negative Int = deriveNegative
                           _ : Negative (Int, Int) = PairwiseNegative
                       in (start - current, end - start)
                  let splitLineDelta = startLine - currentLine
                  let splitColDelta =  if splitLineDelta <= 0
                                       then startCol - currentCol
                                       else startCol
                  putLines outFile 
                         $ process (Plain     !(getLinesCols inFile 
                                                   ( splitLineDelta
                                                   , splitColDelta)))
                  putLines outFile
                         $ process (Decorated !(getLinesCols inFile 
                                                         decoratedRange)
                                               decor)
                  engine inFile outFile end rest

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
  
  let decs = toList fmd.semanticHighlighting 
  ?hole
  
  putStrLn "Katla v0.0"
