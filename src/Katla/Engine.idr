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
import Katla.LaTeX


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

ship : (output : File) -> Maybe Decoration -> (outputChars : SnocList Char) -> IO ()
ship output decor outputChars = when (isSnoc outputChars) $ do
   let decorated = annotate decor (toString outputChars)
   _ <- fPutStr output decorated
   pure ()

processLine : (output : File)
           -> PosMap ASemanticDecoration
           -> (currentDecor  : Maybe Decoration)
           -> (currentPos    : (Int, Int))
           -> (endPos : Maybe (Int, Int))
           -> (remainingLine : List Char)
           -> (currentOutput : SnocList Char)
           -> IO (Maybe Decoration, (Int, Int))
processLine output posMap currentDecor currentPos@(currentRow, currentCol) endPos cs currentOutput
  = case (isNotEndOfLine cs, maybe True (currentPos <) endPos) of
      (Nothing, _) => do
        let nextPos = (currentRow + 1, 0)
        ship output currentDecor currentOutput
        _ <- fPutStrLn output ""
        pure (currentDecor, nextPos)
      (Just _         , False) => do
        ship output currentDecor currentOutput
        _ <- fPutStrLn output ""
        pure (currentDecor, currentPos)
      (Just (c , rest), True) => do
        let (currentRow, currentCol) = currentPos
            nextPos = (currentRow, currentCol + 1)
            decor   = findDecoration currentPos posMap
        if decor == currentDecor
         then processLine output posMap currentDecor nextPos endPos rest (snocEscape currentOutput c)
         else do ship output currentDecor currentOutput
                 processLine output posMap decor nextPos endPos rest (snocEscape [<] c)

engineWithDecor : (input, output : File)
       -> PosMap ASemanticDecoration
       -> Maybe Decoration -> (Int, Int) -> IO ()
engineWithDecor input output posMap currentDecor currentPos
  = when (not !(fEOF input)) $ do
      Right str <- fGetLine input
        | Left err => pure ()
      (nextDecor, nextPos) <- processLine output posMap currentDecor currentPos Nothing
                              (fastUnpack str) [<]
      engineWithDecor input output posMap nextDecor nextPos

export
record ListingRange where
  constructor MkListingRange
  startRow, startCol,
  endRow, endCol : Int

export
RowRangeByOffset : (offset, before, after : Int) -> ListingRange
RowRangeByOffset offset before after = MkListingRange
  { startRow = offset - before
  , endRow = offset + after + 1
  , startCol = 0
  , endCol = 0}

export
RangeByOffsetAndCols : (offset, after,startCol,endCol : Int) -> ListingRange
RangeByOffsetAndCols offset after startCol endCol =
  let row = offset + after
  in MkListingRange
  { startRow = row
  , startCol = startCol
  , endRow   = row
  , endCol   = endCol
  }

(.start),(.end) : ListingRange -> (Int, Int)
range.start = (range.startRow, range.startCol)
range.end   = (range.endRow, range.endCol)

engineWithRange : (input, output : File)
       -> PosMap ASemanticDecoration -> ListingRange
       -> Maybe Decoration -> (Int, Int) -> IO ()
engineWithRange input output posMap rowRange currentDecor currentPos
  = when (not !(fEOF input)) $ do
      Right str <- fGetLine input
        | Left err => pure ()
      (nextDecor, nextPos) <- (
         if rowRange.startRow <= fst currentPos && currentPos < rowRange.end
         then let (decor, startPos, relevantLine) =
                   if rowRange.startRow == fst currentPos
                   then ( Nothing
                        , (fst currentPos, rowRange.startCol)
                        , drop (cast rowRange.startCol) (fastUnpack str))
                   else (currentDecor, currentPos, fastUnpack str)
              in processLine output posMap decor startPos (Just rowRange.end)
                                    relevantLine [<]
         else pure (Nothing, (fst currentPos + 1, 0)))
      engineWithRange input output posMap rowRange nextDecor nextPos


export
engine : (input, output : File)
       -> PosMap ASemanticDecoration
       -> (Int, Int)
       -> IO ()
engine input output posMap = engineWithDecor input output posMap Nothing


record FileHandles where
  constructor MkHandles
  config : Config
  source, output : File
  metadata : PosMap ASemanticDecoration

data Error a = ReportedError | Unreported a

exit : IO (Either (Engine.Error a) b)
exit = pure (Left ReportedError)

export
setupFiles : (mconfig : Maybe String) ->
  (msourcefile, mmetadata : String) -> (moutput : Maybe String) ->
  IO (Either (Error Void) FileHandles)
setupFiles mconfig filename metadata moutput = do
  config <- getConfiguration mconfig
  Right source <- openFile filename  Read
  | Left err => do putStrLn "Couldn't open source file: \{filename}."
                   exit
  Just fmd <- coreRun (map Just (readMetadata metadata))
                      (\err => do putStrLn $ show err
                                  pure Nothing)
                      pure
  | Nothing => do putStrLn "Couldn't open metadata file: \{metadata}"
                  exit

  Right output <- maybe (pure $ Right stdout) (\output => openFile output WriteTruncate) moutput
  | Left err => do putStrLn "couldn't open output: "
                   exit


  pure $ Right $ MkHandles
    { config, source, output
    , metadata = fmd.semanticHighlighting
    }

public export
data Snippet = Raw (Maybe ListingRange) | Macro (String, Bool, Maybe ListingRange)

(.listing) : Snippet -> Maybe ListingRange
(Raw mrange          ).listing = mrange
(Macro (_, _, mrange)).listing = mrange

export
katla : (snippet : Maybe Snippet) -> (mconfig : Maybe String) ->
  (msourcefile, mmetadata, moutput : Maybe String) ->
  -- TODO: would be nice to only specify one of source/metadata
  IO ()
katla _       _       Nothing _       _ = putStrLn "Expecting source file to print."
katla _       _       _       Nothing _ = putStrLn "Expecting metadata file to output."
-- Generate a fully formed LaTeX file
katla Nothing mconfig (Just filename) (Just metadata) moutput = do
  Right files <- setupFiles mconfig filename metadata moutput
  | Left ReportedError => pure ()

  Right _ <- fPutStrLn files.output (standalonePre files.config)
  | Left err => putStrLn "Error while generating preamble: \{show err}"
  engine files.source files.output files.metadata (0,0)
  Right _ <- fPutStrLn files.output standalonePost
  | Left err => putStrLn "Error while generating preamble: \{show err}"
  closeFile files.output
-- Generate only the listing code
katla (Just snippet) mconfig (Just filename) (Just metadata) moutput = do
  Right files <- setupFiles mconfig filename metadata moutput
  | Left ReportedError => pure ()
  case snippet of
    Raw _ => pure ()
    Macro (name, inline, mrange) => do -- TODO: validate macro name, perhaps when parsing
      Right _ <- fPutStrLn files.output
        ((ifThenElse inline makeInlineMacroPre makeMacroPre) name)
      | Left err => putStrLn
        "Error while generating macro name \{name}: \{show err}"
      pure ()
  case snippet.listing of
    Nothing    => engine          files.source files.output files.metadata
                                         (0,0)
    Just range => engineWithRange files.source files.output files.metadata
                                  range Nothing (0,0)
  case snippet of
    Raw _ => pure ()
    Macro (name, inline, mrange) => do
      Right _ <- fPutStrLn files.output $ ifThenElse inline makeInlineMacroPost makeMacroPost
      | Left err => putStrLn
        "Error while generating macro name \{name}: \{show err}"
      pure ()

  closeFile files.output
