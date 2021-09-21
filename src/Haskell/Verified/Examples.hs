module Haskell.Verified.Examples
  ( parse,
    tryLoadImplicitCradle,
    Module (..),
    ModuleInfo (..),
    Example (..),
    examples,
    exampleFromText,
    Comment (..),
    verify,
    verifyExample,
    ExampleResult (..),
    Reporter (..),
    report,
    shimModuleWithImports,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Data.Foldable as Foldable
import qualified Data.List
import qualified Data.Text.IO
import qualified HIE.Bios.Cradle
import qualified HIE.Bios.Environment
import qualified HIE.Bios.Flags
import qualified HIE.Bios.Types
import Haskell.Verified.Examples.Internal
import qualified Haskell.Verified.Examples.Reporter.Stdout as Reporter.Stdout
import Haskell.Verified.Examples.Verified (Verified (..))
import qualified Language.Haskell.Exts as LHE
import qualified Language.Haskell.Exts.Comments as LHE.Comments
import qualified Language.Haskell.Exts.Lexer as LHE.Lexer
import qualified Language.Haskell.Exts.Parser as LHE.Parser
import qualified Language.Haskell.Exts.SrcLoc as LHE.SrcLoc
import qualified Language.Haskell.Exts.Syntax as LHE.Syntax
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Unsafe as Hint.Unsafe
import NriPrelude
import qualified Paths_haskell_verified_examples as DataPath
import qualified System.IO
import qualified Text.Read
import qualified Prelude

verify :: Module -> Prelude.IO (List ExampleResult)
verify Module {comments, moduleInfo} =
  withContext moduleInfo comments <| \maybeContext ->
    comments
      |> examples
      |> Prelude.traverse (verifyExample moduleInfo maybeContext)

verifyExample :: ModuleInfo -> Maybe Context -> Example -> Prelude.IO ExampleResult
verifyExample modInfo maybeContext example =
  case example of
    VerifiedExample _ code -> do
      result <-
        Prelude.unlines code
          |> eval modInfo maybeContext
      case result of
        Prelude.Left err ->
          Prelude.pure (ExampleVerifyFailed example err)
        Prelude.Right execResult ->
          ExampleVerifySuccess example execResult
            |> Prelude.pure
    UnverifiedExample _ code ->
      NoExampleResult
        |> ExampleVerifySuccess example
        |> Prelude.pure

preloadPaths :: Prelude.IO (List Prelude.FilePath)
preloadPaths = Prelude.traverse DataPath.getDataFileName paths
  where
    paths =
      [ "src/Haskell/Verified/Examples/RunTime.hs",
        "src/Haskell/Verified/Examples/Verified.hs"
      ]

shimModuleWithImports :: List Text -> ModuleInfo
shimModuleWithImports imports =
  ModuleInfo
    { moduleName = Nothing,
      moduleSource = LHE.SrcLoc.noSrcSpan,
      languageExtensions = [],
      imports = List.map makeSimpleImport imports,
      importPaths = [],
      packageDbs = []
    }

makeSimpleImport :: Text -> Hint.ModuleImport
makeSimpleImport name = Hint.ModuleImport (Text.toList name) Hint.NotQualified Hint.NoImportList

makeImport :: LHE.Syntax.ImportDecl LHE.SrcLoc.SrcSpanInfo -> Hint.ModuleImport
makeImport importDecl =
  Hint.ModuleImport
    { Hint.modName = getModName <| LHE.Syntax.importModule importDecl,
      Hint.modQual = modQual,
      Hint.modImp = importList
    }
  where
    getName (LHE.Syntax.Ident _ n) = n
    getName (LHE.Syntax.Symbol _ n) = "(" ++ n ++ ")"
    getModName (LHE.Syntax.ModuleName _ n) = n
    getCName (LHE.Syntax.VarName _ n) = getName n
    getCName (LHE.Syntax.ConName _ n) = getName n
    modQual = case (LHE.Syntax.importQualified importDecl, LHE.Syntax.importAs importDecl) of
      -- import Foo
      (False, Nothing) -> Hint.NotQualified
      -- import Foo as Bar
      (False, Just name) -> Hint.ImportAs <| getModName name
      -- import qualified Foo
      (True, Nothing) -> Hint.QualifiedAs Nothing
      -- import qualified Foo as Bar
      (True, Just name) -> Hint.QualifiedAs (Just <| getModName name)

    importList = case LHE.Syntax.importSpecs importDecl of
      Nothing -> Hint.NoImportList
      Just (LHE.Syntax.ImportSpecList _ False names) -> Hint.ImportList <| List.map importToString names
      Just (LHE.Syntax.ImportSpecList _ True names) -> Hint.HidingList <| List.map importToString names

    importToString (LHE.Syntax.IVar _ n) = getName n
    importToString (LHE.Syntax.IAbs _ _ n) = getName n
    importToString (LHE.Syntax.IThingAll _ n) = getName n ++ "(..)"
    importToString (LHE.Syntax.IThingWith _ n ns) = getName n ++ "(" ++ (List.concat <| List.intersperse "," (List.map getCName ns)) ++ ")"

eval ::
  ModuleInfo ->
  Maybe Context ->
  Prelude.String ->
  Prelude.IO (Prelude.Either Hint.InterpreterError Verified)
eval moduleInfo maybeContext s = do
  let modulePath = moduleFilePath moduleInfo
  let interpreter = case packageDbs moduleInfo of
        [] -> Hint.runInterpreter
        _ -> Hint.Unsafe.unsafeRunInterpreterWithArgs <| List.map Text.toList <| packageDbs moduleInfo

  interpreter <| do
    preload <- Hint.lift preloadPaths

    -- TODO: Throw nice "unrecognized extension" error instead of ignoring here
    let langs = List.filterMap (\ex -> Text.Read.readMaybe <| Text.toList ex) (languageExtensions moduleInfo)
    let searchPaths = List.map Text.toList <| importPaths moduleInfo
    Hint.set [Hint.languageExtensions Hint.:= langs, Hint.searchPath Hint.:= searchPaths]

    [ if modulePath == ""
        then []
        else [modulePath],
      case maybeContext of
        Nothing -> []
        Just Context {contextModulePath} -> [contextModulePath],
      preload
      ]
      |> List.concat
      |> Hint.loadModules

    case moduleName moduleInfo of
      Just name -> Hint.setTopLevelModules [Text.toList name]
      Nothing -> Prelude.return ()

    let exampleImports =
          [ Just "Haskell.Verified.Examples.RunTime",
            Just "Haskell.Verified.Examples.Verified",
            Maybe.map contextModuleName maybeContext
          ]
            |> List.filterMap identity
            |> List.map makeSimpleImport

    Hint.setImportsF (exampleImports ++ imports moduleInfo)
    Hint.interpret s (Hint.as :: Verified)

trimPrefix :: Text -> Text -> Maybe Text
trimPrefix prefix text
  | Text.startsWith prefix text = Just <| Text.dropLeft (Text.length prefix) text
  | Prelude.otherwise = Nothing

getSearchPaths :: List Text -> List Text
getSearchPaths = List.filterMap <| trimPrefix "-i"

getDefaultLanguageExtensions :: List Text -> List Text
getDefaultLanguageExtensions = List.filterMap <| trimPrefix "-X"

getPackageDbs :: List Text -> List Text
getPackageDbs options = List.concat [[l, r] | (l, r) <- Prelude.zip options (List.drop 1 options), l == "-package-db"]

exampleFromText :: Prelude.String -> Example
exampleFromText val =
  toExample emptySrcSpan (Prelude.lines val)

parse :: Prelude.FilePath -> Prelude.IO Module
parse path = do
  parsed <- parseFileWithComments path
  case parsed of
    LHE.Parser.ParseOk ok -> Prelude.pure (toModule ok)
    LHE.Parser.ParseFailed x msg ->
      Debug.todo (Debug.toString x ++ Debug.toString msg)

-- Parses the file for imports / extensions / comments, but also will attempt to find the cradle for project default extensions and module directories
tryLoadImplicitCradle :: Prelude.FilePath -> Module -> Prelude.IO Module
tryLoadImplicitCradle path mod = do
  cradle <- HIE.Bios.Cradle.loadImplicitCradle path
  cradleResult <- HIE.Bios.Flags.getCompilerOptions path cradle

  case cradleResult of
    HIE.Bios.Types.CradleSuccess componentOptions -> do
      let opts = List.map Text.fromList <| HIE.Bios.Types.componentOptions componentOptions
      let searchPaths = getSearchPaths opts
      let packageDbs = getPackageDbs opts
      let defaultExtensions = getDefaultLanguageExtensions opts

      let modInfo = moduleInfo mod

      let exModInfo =
            modInfo
              { languageExtensions = defaultExtensions ++ languageExtensions modInfo,
                importPaths = searchPaths,
                packageDbs = packageDbs
              }

      Prelude.return mod {moduleInfo = exModInfo}
    err ->
      let _ = Debug.log "Failed to load cradle with error" err
       in Prelude.return mod

examples :: List Comment -> List Example
examples =
  List.filterMap
    ( \c ->
        case c of
          ContextBlockComment _ _ -> Nothing
          CodeBlockComment example -> Just example
    )

contextBlocks :: List Comment -> List Prelude.String
contextBlocks =
  List.concatMap
    ( \c ->
        case c of
          ContextBlockComment _ context -> context
          CodeBlockComment _ -> []
    )

data Context = Context
  { contextModulePath :: Prelude.FilePath,
    contextModuleName :: Text
  }

withContext :: ModuleInfo -> List Comment -> (Maybe Context -> Prelude.IO a) -> Prelude.IO a
withContext modInfo comments go = do
  let contextModuleName = "HaskellVerifiedExamplesContext"
  case contextBlocks comments of
    [] -> go Nothing
    xs ->
      withTempFile
        ( \path handle -> do
            _ <- System.IO.hPutStrLn handle ("module " ++ Text.toList contextModuleName ++ " where")
            _ <-
              modInfo
                |> imports
                |> List.map printImport
                |> Prelude.traverse (System.IO.hPutStrLn handle)
            xs
              |> Prelude.unlines
              |> System.IO.hPutStr handle
            Prelude.pure ()
        )
        (\contextModulePath -> go (Just Context {contextModulePath, contextModuleName}))

printImport :: Hint.ModuleImport -> Prelude.String
printImport m =
  "import "
    ++ ( case Hint.modQual m of
           Hint.NotQualified -> Hint.modName m
           Hint.ImportAs q -> Hint.modName m ++ " as " ++ q
           Hint.QualifiedAs Nothing -> "qualified " ++ Hint.modName m
           Hint.QualifiedAs (Just q) -> "qualified " ++ Hint.modName m ++ " as " ++ q
       )
    ++ ( case Hint.modImp m of
           Hint.NoImportList -> ""
           Hint.ImportList l -> " (" ++ Data.List.intercalate "," l ++ ")"
           Hint.HidingList l -> " hiding (" ++ Data.List.intercalate "," l ++ ")"
       )

toModule ::
  ( LHE.Syntax.Module LHE.SrcLoc.SrcSpanInfo,
    List LHE.Comments.Comment
  ) ->
  Module
toModule parsed =
  case parsed of
    (LHE.Syntax.Module moduleSource moduleHead pragmas imports _, cs) ->
      let moduleName = case moduleHead of
            (Just (LHE.Syntax.ModuleHead _ (LHE.Syntax.ModuleName _ name) _ _)) -> Just <| Text.fromList name
            Nothing -> Nothing
          languageExtensions = [Text.fromList n | LHE.Syntax.LanguagePragma _ ns <- pragmas, (LHE.Syntax.Ident _ n) <- ns]
       in Module
            { moduleInfo =
                ModuleInfo
                  { moduleName,
                    moduleSource,
                    languageExtensions,
                    imports = List.map makeImport imports,
                    importPaths = [],
                    packageDbs = []
                  },
              comments = toComments cs
            }
    _ ->
      Debug.todo "TODO unsupported module type"

toComments :: List LHE.Comments.Comment -> List Comment
toComments cs =
  cs
    |> mergeComments [] False
    |> List.filterMap
      ( \(ct, comments) ->
          case ct of
            PlainText -> Nothing
            CodeBlock ->
              comments
                |> List.map (commentValue >> Prelude.dropWhile (/= '>') >> Prelude.drop 2)
                |> toExample (commentsSrcSpan comments)
                |> CodeBlockComment
                |> Just
            ContextBlock ->
              comments
                |> List.map commentValue
                |> Data.List.tail
                |> Data.List.init
                |> List.map (Prelude.drop 1)
                |> ContextBlockComment (commentsSrcSpan comments)
                |> Just
      )

data CommentType = CodeBlock | PlainText | ContextBlock
  deriving (Show, Eq)

mergeComments ::
  List (CommentType, List LHE.Comments.Comment) ->
  Bool ->
  List LHE.Comments.Comment ->
  List (CommentType, List LHE.Comments.Comment)
mergeComments acc _ [] = List.reverse acc
mergeComments acc isInContext (next : restNext) =
  let nextCt = commentType next
      stillInContext = if isInContext then nextCt /= ContextBlock else nextCt == ContextBlock
      newAcc = case acc of
        [] -> [(nextCt, [next])]
        (prevCt, prev) : restPrev ->
          if isInContext || prevCt == nextCt
            then (prevCt, prev ++ [next]) : restPrev
            else (nextCt, [next]) : acc
   in mergeComments newAcc stillInContext restNext

commentType :: LHE.Comments.Comment -> CommentType
commentType (LHE.Comments.Comment _ _ text) =
  if hasArrow text
    then CodeBlock
    else
      if hasAt text
        then ContextBlock
        else PlainText

hasAt text = Text.trim (Text.fromList text) == "@"

hasArrow text =
  Text.startsWith " > " (Text.fromList text)
    || Text.trim (Text.fromList text) == ">"

concatComment :: LHE.Comments.Comment -> LHE.Comments.Comment -> LHE.Comments.Comment
concatComment commentA@(LHE.Comments.Comment _ srcSpanA a) commentB@(LHE.Comments.Comment _ srcSpanB b) =
  LHE.Comments.Comment True (LHE.SrcLoc.mergeSrcSpan srcSpanA srcSpanB) (a ++ "\n" ++ b)

commentValue :: LHE.Comments.Comment -> Prelude.String
commentValue (LHE.Comments.Comment _ _ a) = a

commentsSrcSpan :: List LHE.Comments.Comment -> LHE.SrcLoc.SrcSpan
commentsSrcSpan [] = emptySrcSpan
commentsSrcSpan (LHE.Comments.Comment _ first _ : rest) =
  List.foldl
    (\(LHE.Comments.Comment _ srcSpan _) acc -> LHE.SrcLoc.mergeSrcSpan acc srcSpan)
    first
    rest

emptySrcSpan :: LHE.SrcLoc.SrcSpan
emptySrcSpan = LHE.SrcLoc.mkSrcSpan LHE.SrcLoc.noLoc LHE.SrcLoc.noLoc

toExample :: LHE.SrcLoc.SrcSpan -> List Prelude.String -> Example
toExample srcSpan source =
  case LHE.Lexer.lexTokenStream (Prelude.unlines source) of
    LHE.Parser.ParseOk tokens ->
      if Foldable.any ((== LHE.Lexer.VarSym "==>") << LHE.Lexer.unLoc) tokens
        then VerifiedExample srcSpan source
        else UnverifiedExample srcSpan source
    LHE.Parser.ParseFailed _ msg ->
      let _ = Debug.log "msg" msg
       in Debug.todo "TODO"

parseFileWithComments ::
  Prelude.FilePath ->
  Prelude.IO
    ( LHE.Parser.ParseResult
        ( LHE.Syntax.Module LHE.SrcLoc.SrcSpanInfo,
          List LHE.Comments.Comment
        )
    )
parseFileWithComments path =
  LHE.parseFileWithComments (LHE.defaultParseMode {LHE.parseFilename = path, LHE.extensions = [LHE.EnableExtension LHE.CPP]}) path

data Reporter
  = Stdout
  | -- | TODO
    Junit
  | LogFile
  deriving (Eq)

report :: List Reporter -> List (ModuleInfo, List ExampleResult) -> Prelude.IO ()
report reporters results =
  [ if List.member Stdout reporters
      then Just (Reporter.Stdout.report System.IO.stdout results)
      else Nothing
  ]
    |> List.filterMap identity
    |> Async.mapConcurrently_ identity

withTempFile ::
  (System.IO.FilePath -> System.IO.Handle -> Prelude.IO ()) ->
  (Prelude.FilePath -> Prelude.IO a) ->
  Prelude.IO a
withTempFile before go = do
  (path, handle) <-
    System.IO.openTempFile "/tmp" "HaskellVerifiedExamples.hs"
  _ <- before path handle
  System.IO.hClose handle
  go path
