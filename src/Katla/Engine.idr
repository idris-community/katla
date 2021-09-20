module Katla.Engine

import System
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
import Katla.HTML


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

public export
Position : Type
Position = (Int, Int)

export
nextRow : Position -> Position
nextRow (row, _) = (row + 1, 0)

export
nextColumn : Position -> Position
nextColumn (row, col) = (row, col + 1)

findDecoration : Position -> PosMap ASemanticDecoration -> Maybe Decoration
findDecoration pos@(row, col) posMap =
  case dominators ((row, col), (row, col+1)) posMap of
   []              => Nothing
   (d :: ds)       => Just $ pickSmallest (d ::: ds)

toString : SnocList Char -> String
toString sx = (fastPack $ sx <>> [])

snocEscape : (escape : Char -> List Char) ->
             (outputChars : SnocList Char) -> (new : Char) -> SnocList Char
snocEscape escape sx c = sx <>< escape c

||| True if input starts with EOL
isNotEndOfLine : List Char -> Maybe (Char, List Char)
isNotEndOfLine []           = Nothing
isNotEndOfLine ('\r' :: _ ) = Nothing
isNotEndOfLine ('\n' :: _ ) = Nothing
isNotEndOfLine (x    :: xs) = Just (x, xs)

ship : (output : File) ->
       Driver ->
       Maybe Decoration -> (outputChars : SnocList Char) -> IO ()
ship output driver decor outputChars = when (isSnoc outputChars) $ do
   let decorated = driver.annotate decor (toString outputChars)
   ignore $ fPutStr output decorated

processLine : (output : File)
           -> (meta : PosMap ASemanticDecoration)
           -> Driver
           -> (currentDecor  : Maybe Decoration)
           -> (currentPos    : Position)
           -> (endPos : Maybe Position)
           -> (remainingLine : List Char)
           -> (currentOutput : SnocList Char)
           -> IO (Maybe Decoration, Position)
processLine output meta driver currentDecor currentPos endPos cs currentOutput
  = case (isNotEndOfLine cs, maybe True (currentPos <) endPos) of
      -- We've reached the end of the line: output and return
      (Nothing, _) => do
        let nextPos = nextRow currentPos
        ship output driver currentDecor currentOutput
        ignore $ fPutStrLn output (snd driver.line)
        pure (currentDecor, nextPos)
      -- We're past the caller-provided end position: output and return
      (Just _         , False) => do
        ship output driver currentDecor currentOutput
        ignore $ fPutStrLn output ""
        pure (currentDecor, currentPos)
      -- We're still in bounds and have found a new character
      -- Assuming decorations may overlap, we need to check whether there is a
      -- new one or whether we can keep munching the line using the same decor.
      -- If we were willing to assume decorations are non-overlapping we could
      -- just return the size of the decorated chunk in `findDecoration` and
      -- grab it whole here.
      (Just (c , rest), True) => do
        let nextPos = nextColumn currentPos
            decor   = findDecoration currentPos meta
        if decor == currentDecor
         then let c = snocEscape driver.escape currentOutput c in
              processLine output meta driver currentDecor nextPos endPos rest c
         else do ship output driver currentDecor currentOutput
                 let c = snocEscape driver.escape [<] c
                 processLine output meta driver decor nextPos endPos rest c

engineWithDecor
  : (input, output : File)
  -> (lineNumberWidth : Nat) -- width of the largest line number e.g. 3 for 999
  -> (meta : PosMap ASemanticDecoration)
  -> Driver
  -> Maybe Decoration -> Position -> IO ()
engineWithDecor input output lineNumberWidth meta driver currentDecor currentPos
  = when (not !(fEOF input)) $ do
      Right str <- fGetLine input
        | Left err => pure ()
      -- if we're starting a new line, output the corresponding marker
      when (snd currentPos == 0) $
        ignore $ fPutStr output
               $ fst driver.line lineNumberWidth
               $ cast $ fst currentPos
      -- then process the line
      next <- processLine output meta driver currentDecor currentPos Nothing
                (fastUnpack str) [<]
      let (nextDecor, nextPos) = next
      engineWithDecor input output lineNumberWidth meta driver nextDecor nextPos

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

(.start),(.end) : ListingRange -> Position
range.start = (range.startRow, range.startCol)
range.end   = (range.endRow, range.endCol)


engineWithRange
  : (input, output : File)
  -> (lineNumberWidth : Nat) -- width of the largest line number e.g. 3 for 999
  -> (meta : PosMap ASemanticDecoration)
  -> Driver
  -> ListingRange
  -> Maybe Decoration -> Position -> IO ()
engineWithRange input output lineNumberWidth meta driver rowRange currentDecor currentPos
  = when (not !(fEOF input)) $ do
      Right str <- fGetLine input
        | Left err => pure ()
      (nextDecor, nextPos) <- (
         -- If the current line in the file intersects with the range
         -- then process the line and otherwise just go to the next one
         if rowRange.startRow <= fst currentPos && currentPos < rowRange.end
         then do
           let (decor, startPos, relevantLine) =
                   if rowRange.startRow == fst currentPos
                     then ( Nothing
                          , (fst currentPos, rowRange.startCol)
                          , drop (cast rowRange.startCol) (fastUnpack str))
                     else (currentDecor, currentPos, fastUnpack str)
           let endPos = Just rowRange.end
           ignore $ fPutStr output
                  $ fst driver.line lineNumberWidth
                  $ cast $ fst currentPos
           processLine output meta driver decor startPos endPos relevantLine [<]
         else pure (Nothing, nextRow currentPos))
      -- stop processing the file as soon as we're beyond the range
      unless (rowRange.end < nextPos) $
        engineWithRange input output lineNumberWidth meta driver rowRange nextDecor nextPos


export
engine : (input, output : File)
       -> (lineNumberWidth : Nat)
       -> (meta : PosMap ASemanticDecoration)
       -> Driver
       -> Position
       -> IO ()
engine input output lnw meta driver
  = engineWithDecor input output lnw meta driver Nothing

record FileHandles where
  constructor MkHandles
  config : Config
  source, output : File
  metadata : PosMap ASemanticDecoration

data Error a = ReportedError | Unreported a

exit : IO (Either (Engine.Error a) b)
exit = pure (Left ReportedError)

export
setupFiles : Backend ->
  (mconfig : Maybe String) ->
  (msourcefile, mmetadata : String) ->
  (moutput : Maybe String) ->
  IO (Either (Error Void) FileHandles)
setupFiles backend mconfig filename metadata moutput = do
  config <- getConfiguration backend mconfig
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
data Snippet
  = Raw (Maybe ListingRange)
  | Macro (String, Bool, Maybe ListingRange)

(.listing) : Snippet -> Maybe ListingRange
(Raw mrange          ).listing = mrange
(Macro (_, _, mrange)).listing = mrange

mkDriver : Backend -> (Config -> Driver)
mkDriver LaTeX = LaTeX.mkDriver
mkDriver HTML  = HTML.mkDriver

export
katla : (backend : Backend) ->
        (snippet : Maybe Snippet) ->
        (mconfig : Maybe String) ->
        (msourcefile, mmetadata, moutput : Maybe String) ->
        -- TODO: would be nice to only specify one of source/metadata
        IO ()
katla _ _       _       Nothing _       _
  = putStrLn "Expecting source file to print."
katla _ _       _       _       Nothing _
  = putStrLn "Expecting metadata file to output."
-- Generate a fully formed file
katla backend Nothing mconfig (Just filename) (Just metadata) moutput = do
  Right files <- setupFiles backend mconfig filename metadata moutput
    | Left ReportedError => pure ()

  let error : String -> IO ()
      error str = do putStrLn "Error while \{str}"
                     closeFile files.output
                     exitFailure

  let driver = mkDriver backend files.config
  let (standalonePre, standalonePost) = driver.standalone

  Right _ <- fPutStrLn files.output standalonePre
    | Left err => error "generating preamble: \{show err}"
  Right content <- readFile filename
     | Left _  => pure ()
  let lnw = length $ show $ length $ lines content
  engine files.source files.output lnw files.metadata driver (0,0)
  Right _ <- fPutStrLn files.output standalonePost
    | Left err => error "generating preamble: \{show err}"
  closeFile files.output
-- Generate only the listing code
katla backend (Just snippet) mconfig (Just filename) (Just metadata) moutput = do
  Right files <- setupFiles backend mconfig filename metadata moutput
    | Left ReportedError => exitFailure

  let error : String -> IO ()
      error str = do putStrLn "Error while \{str}"
                     closeFile files.output
                     exitFailure

  let driver = mkDriver backend files.config
  case snippet of
    Raw _ => pure ()
    Macro (name, inline, mrange) => do -- TODO: validate macro name, perhaps when parsing
      let (pre, _) = ifThenElse inline driver.inlineMacro driver.blockMacro
      Right _ <- fPutStrLn files.output (pre name)
        | Left err => error "generating macro name \{name}: \{show err}"
      pure ()
  case snippet.listing of
    Nothing    =>
      do Right content <- readFile filename
           | Left _  => pure ()
         let lnw = length $ show $ length $ lines content
         engine files.source files.output lnw files.metadata driver (0,0)
    Just range =>
      do let lnw = length $ show range.endRow
         engineWithRange files.source files.output lnw files.metadata driver range Nothing (0,0)
  case snippet of
    Raw _ => pure ()
    Macro (name, inline, mrange) => do
      let (_, post) = ifThenElse inline driver.inlineMacro driver.blockMacro
      Right _ <- fPutStrLn files.output post
        | Left err => error "generating macro name \{name}: \{show err}"
      pure ()

  closeFile files.output
