module Katla.Pandoc

import Control.Monad.State

import Data.SortedMap
import Data.SortedSet
import Data.String

import Language.JSON
import System
import System.Directory
import System.Random

%default total

updateJSON : String -> (JSON -> JSON) -> JSON -> JSON
updateJSON key f json = update (\case Nothing => Just (f JNull); Just x => Just (f x)) key json

findAttr : (key : String) ->
           (default_ : String) ->
           List JSON ->
           String
findAttr key default_ [] = default_
findAttr key default_ (JArray [JString k, JString v] :: attrs) =
    if k == key
        then v
        else findAttr key default_ attrs
findAttr key default_ (_ :: attrs) = findAttr key default_ attrs

findPackages : JSON -> List String
findPackages doc = do
    let Just (JArray packages) = lookup "meta" doc >>= lookup "idris2-packages" >>= lookup "c"
        | _ => []
    JObject [("t", JString "MetaInlines"), ("c", JArray [JObject [("t", JString "Str"), ("c", JString package)]])] <- packages
        | _ => []
    pure package

data DisplayType = Block | Inline

data CodeType = Decls | Expr String

record Snippet where
    constructor MkSnippet
    code : String
    displayType : DisplayType
    codeType : CodeType
    hide : Bool
    file : String
    namespace_ : String

parseSnippet : JSON -> Maybe Snippet
parseSnippet (JObject [
    ("t", JString t),
    ("c", JArray [JArray [_, JArray (JString "idr" :: clss), JArray attrs], JString code])
  ]) = do
    displayType <- case t of
        "CodeBlock" => pure Block
        "Code" => pure Inline
        _ => Nothing

    let codeType = do
        let exprType = findAttr "type" "?" attrs
        if elem (JString "decls") clss || elem (JString "decl") clss
            then Decls
            else if elem (JString "expr") clss || exprType /= "?"
              then Expr exprType
              else case displayType of
                  Block => Decls
                  Inline => Expr exprType

    pure $ MkSnippet
        code
        displayType
        codeType
        (elem (JString "hide") clss)
        (findAttr "file" "Main" attrs)
        (findAttr "namespace" "" attrs)
parseSnippet json = Nothing

traverseSnippets : Monad m => (Snippet -> m JSON) -> JSON -> m JSON
traverseSnippets f json = traverseJSON (\x => maybe (pure x) f (parseSnippet x)) json

traverseSnippets_ : Monad m => (Snippet -> m ()) -> JSON -> m ()
traverseSnippets_ f json = traverseJSON_ (\x => maybe (pure ()) f (parseSnippet x)) json

collateCode : JSON -> SnocList Snippet
collateCode doc = execState [<] $ traverseSnippets_ (\snippet => modify (:< snippet)) doc

covering
writeCode : (dir : String) ->
            (snippets : List Snippet) ->
            IO (List (Nat, Nat))
writeCode dir snippets = do
    (files, ranges) <- runStateT SortedMap.empty $ traverse (\snippet => do
        let fileName = "\{dir}/\{snippet.file}.idr"

        let indentDepth = 0

        indentDepth <- if snippet.namespace_ /= ""
            then do
                ignore $ writeBlock fileName indentDepth "namespace \{snippet.namespace_}"
                pure $ 1 + indentDepth
            else pure indentDepth

        indentDepth <- case snippet.codeType of
            Decls => pure indentDepth
            Expr exprType => do
                let varName = "x\{show $ cast {to = Bits32} !(randomIO {a = Int32})}"
                ignore $ writeBlock fileName indentDepth "\{varName} : \{exprType}\n\{varName} ="
                pure $ 1 + indentDepth

        writeBlock fileName indentDepth snippet.code
      ) snippets
    traverse_ (\(file, _) => closeFile file) files
    pure ranges
  where
    indent : Nat -> List String -> List String
    indent indentDepth block = do
        let indentPrefix = replicate (4 * indentDepth) ' '
        map (\case "" => ""; line => indentPrefix ++ line) block

    getFile : String -> StateT (SortedMap String (File, Nat)) IO (File, Nat)
    getFile fileName = do
        files <- get
        let Nothing = lookup fileName files
            | Just file => pure file

        Right file <- openFile fileName WriteTruncate
            | Left fileError => die "Error writing Idris code to file: \{show fileError}"

        modify $ insert fileName (file, 1)
        pure (file, 1)

    writeBlock : (fileName : String) ->
                 (indentDepth : Nat) ->
                 (block : String) ->
                 StateT (SortedMap String (File, Nat)) IO (Nat, Nat)
    writeBlock fileName indentDepth block = do
        let ls = indent indentDepth $ lines block
        let lineCount = length ls
        (file, start) <- getFile fileName

        -- Round trip of `unlines . lines` not identiy
        -- This might introduce a new trailing newline
        -- Trailing newline important for consistency of line counting
        Right () <- fPutStrLn file $ unlines ls
            | Left fileError => die "Error writing Idris code to file: \{show fileError}"

        modify $ updateExisting (mapSnd (1 + lineCount +)) fileName
        pure (start, minus lineCount 1)

covering
checkCode : (dir : String) ->
            (packages : List String) ->
            (snippets : List Snippet) ->
            IO ()
checkCode dir packages snippets = do
    let fileNames = SortedSet.fromList $ map file snippets
    for_ fileNames $ \fileName => do
        (_, 0) <- run $ ["idris2", "-c", "\{dir}/\{fileName}.idr"] ++ (packages >>= (\package => ["-p", package]))
            | (msg, err) => die $ "Error checking Idris code (exit code: \{show err})\n" ++ msg
        pure ()

addPandocHeaderIncludes : List String -> JSON -> JSON
addPandocHeaderIncludes newHeaders doc = do
    let newHeaders = map (\newHeader => JObject [
            ("t", JString "MetaBlocks"),
            ("c", JArray [JObject [
                ("t", JString "RawBlock"),
                ("c", JArray [JString "tex", JString newHeader])
            ]])
        ]) newHeaders
    update (Just . update (Just . update (Just . (\case
        JArray existingHeaders => JArray $ existingHeaders ++ newHeaders
        existingHeader => JArray $ existingHeader :: newHeaders) .
        maybe (JArray []) id) "c" .
        maybe (JObject [("t", JString "MetaList")]) id) "header-includes" .
        maybe (JObject []) id) "meta" doc

covering
addKatlaHeader : JSON -> IO JSON
addKatlaHeader doc = do
    (katlaHeader, 0) <- run "katla latex preamble"
        | (_, err) => die "Error getting Katla header (exit code: \{show err})"

    pure $ addPandocHeaderIncludes [katlaHeader] doc

covering
formatSnippet : (dir : String) -> Snippet -> (snippetName : String) -> (start : Nat) -> (len : Nat) -> IO (Maybe String, JSON)
formatSnippet dir snippet snippetName start len = do
    let False = snippet.hide
        | True => pure (Nothing, JObject [("t", JString "Para"), ("c", JArray [])])

    let katlaCmd = case snippet.displayType of
          Block => "katla latex macro \{snippetName} \{dir}/\{snippet.file}.idr build/ttc/*/\{dir}/\{snippet.file}.ttm \{show start} 0 \{show len}"
          Inline => "katla latex macro inline \{snippetName} \{dir}/\{snippet.file}.idr build/ttc/*/\{dir}/\{snippet.file}.ttm \{show start} 0 \{show $ 1 + len} \{show $ 8 + length snippet.code}"
    (out, 0) <- run katlaCmd
        | (_, err) => die "Error running Katla (exit code: \{show err})"

    let out = if snippet.namespace_ /= "" then dedent out else out
    let out = case snippet.codeType of
          Decls => out
          Expr _ => dedent out

    pure (Just out, JObject [
        ("t", JString $ case snippet.displayType of Block => "RawBlock"; Inline => "RawInline"),
        ("c", JArray [ JString "tex", JString "\\\{snippetName}{}"])
    ])
  where
    katlaIndent : String
    katlaIndent = "\\KatlaSpace{}\\KatlaSpace{}\\KatlaSpace{}\\KatlaSpace{}"

    katlaIndentLen : Nat
    katlaIndentLen = length katlaIndent

    dedent : String -> String
    dedent block = unlines $
        map (\line => if isPrefixOf katlaIndent line
            then substr katlaIndentLen (minus (length line) katlaIndentLen) line
            else line
        ) $
        lines block

covering
addKatlaSnippets : (dir : String) -> List Snippet -> List (Nat, Nat) -> JSON -> IO JSON
addKatlaSnippets dir snippets ranges doc = do
    ((headers, _), doc) <- runStateT (the (SnocList String) [<], ranges) $ traverseSnippets (\snippet => do
        (headers, (start, len) :: rest) <- get {stateType = (SnocList String, List (Nat, Nat))}
            | _ => die "katla-pandoc internal error"

        snippetName <- lift genName
        (newHeader, json) <- lift $ formatSnippet dir snippet snippetName start len

        put (
            case newHeader of Nothing => headers; Just newHeader => headers :< newHeader,
            rest
          )

        pure json
      ) doc

    pure $ addPandocHeaderIncludes (cast headers) doc
  where
    alpha : List Char
    alpha = unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    genName : IO String
    genName = do
        suffix <- for (replicate 6 ()) $ \() => rndSelect alpha
        pure "KatlaSnippet\{pack suffix}"

covering
main : IO ()
main = do
    doc <- getLine
    let Just doc = parse doc
        | Nothing => pure ()

    let snippets = cast $ collateCode doc
    let packages = findPackages doc

    let dir = "build/katla"
    ignore $ createDir "build"
    ignore $ createDir dir

    ranges <- writeCode dir snippets
    checkCode dir packages snippets
    doc <- addKatlaHeader doc
    doc <- addKatlaSnippets dir snippets ranges doc

    printLn doc
