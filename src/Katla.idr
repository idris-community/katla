module Katla

import System.File
import Core.FC
import Core.Name
import Core.Core
import Core.Metadata
import Libraries.Data.PosMap
import Data.List.Lazy


data Event 
  = Plain     (List String)
  | Decorated (List String) Decoration

escapeLatex : Char -> String
escapeLatex '\\' = "\\backslash"
escapeLatex '{'  = "\\{"
escapeLatex '}'  = "\\}"
escapeLatex x    = cast x

infixr 4 |>

data EventStream : Type where
  Halt : EventStream
  (|>) : Event -> IO EventStream -> EventStream
      
engine : File -> LazyList ASemanticDecoration -> IO EventStream
engine file []  
   = do Just lines 
              <- do Right line <- fGetLine file 
                      | Left err => ?hole
                    let escapedLine map escapeLatex line
              | Nothing => Nothing
        ?hole010
        pure $ Plain ?engine_rhs_1 
     |> pure   Halt
engine fname (x :: xs)  = ?engine_rhs_3


-- x11names
decorate : Decoration -> String -> String
decorate Typ      str = "{\color{DeepSkyBlue3}" ++ str ++ "}"
decorate Function str = "{\color{Chartreuse4}"  ++ str ++ "}"
decorate Data     str = "{\color{IndianRed1}"   ++ str ++ "}"
decorate Keyword  str = "{\color{Azure4}"       ++ str ++ "}"
decorate Bound    str = "{\color{DarkOrchid3}"  ++ str ++ "}"

process : Event -> String
process (Plain     strs    ) = str 
process (Decorated strs dec) = decorate dec str 

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
  
  ?hole
  
  putStrLn "Katla v0.0"
