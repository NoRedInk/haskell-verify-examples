module Haskell.Verify.Examples
  ( Handler,
    handler,
    CradleInfo (..),
    tryLoadImplicitCradle,
    parse,
    Module (..),
    ModuleInfo,
    Example,
    verify,
    ExampleResult,
    Reporter (..),
    report,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception.Safe as Exception
import qualified Data.Foldable as Foldable
import qualified Data.List
import qualified Data.Text.IO
import qualified HIE.Bios.Cradle
import qualified HIE.Bios.Environment
import qualified HIE.Bios.Flags
import qualified HIE.Bios.Types
import Haskell.Verify.Examples.Internal
import qualified Haskell.Verify.Examples.Reporter.Stdout as Reporter.Stdout
import Haskell.Verify.Examples.Verified (Verified (..))
import qualified Language.Haskell.Exts as LHE
import qualified Language.Haskell.Exts.Comments as LHE.Comments
import qualified Language.Haskell.Exts.Lexer as LHE.Lexer
import qualified Language.Haskell.Exts.Parser as LHE.Parser
import qualified Language.Haskell.Exts.SrcLoc as LHE.SrcLoc
import qualified Language.Haskell.Exts.Syntax as LHE.Syntax
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Unsafe as Hint.Unsafe
import qualified Language.Preprocessor.Cpphs as Cpphs
import NriPrelude
import qualified Paths_haskell_verify_examples as DataPath
import qualified Platform
import qualified System.IO
import qualified Text.Read
import qualified Prelude

data Handler = Handler
  { eval :: CradleInfo -> ModuleInfo -> Maybe Context -> Prelude.String -> Task EvalError Verified,
    loadImplicitCradle ::
      Prelude.FilePath ->
      Task Error (HIE.Bios.Types.Cradle HIE.Bios.Types.ComponentOptions),
    getCompilerOptions ::
      Prelude.FilePath ->
      HIE.Bios.Types.Cradle HIE.Bios.Types.ComponentOptions ->
      Task Error HIE.Bios.Types.ComponentOptions,
    writeTempFile :: List Prelude.String -> Task Error Prelude.FilePath,
    readFile :: Prelude.FilePath -> Task Error Prelude.String,
    runCpphs :: Prelude.FilePath -> Prelude.String -> Task Error Prelude.String
  }

handler :: Prelude.IO Handler
handler = do
  doAnything <- Platform.doAnythingHandler
  let eval a b c d =
        evalIO a b c d
          |> map Ok
          |> Exception.handle
            (\(err :: EvalError) -> Prelude.pure (Err err))
          |> Exception.handle
            (\(err :: Hint.InterpreterError) -> Prelude.pure (Err (InterpreterError err)))
          |> Platform.doAnything doAnything
  let readFile path =
        Prelude.readFile path
          |> map Ok
          |> Exception.handle (ReadFileFailed >> Err >> Prelude.pure)
          |> Platform.doAnything doAnything
  let runCpphs path contents =
        Cpphs.runCpphs Cpphs.defaultCpphsOptions path contents
          |> map Ok
          |> Platform.doAnything doAnything
  let loadImplicitCradle p =
        HIE.Bios.Cradle.loadImplicitCradle p
          |> map Ok
          |> Platform.doAnything doAnything
  let getCompilerOptions p c =
        HIE.Bios.Flags.getCompilerOptions p c
          |> map
            ( \case
                HIE.Bios.Types.CradleSuccess componentOptions -> Ok componentOptions
                HIE.Bios.Types.CradleFail err -> Err (CradleFailed err)
            )
          |> Platform.doAnything doAnything
  let writeTempFile contents =
        writeTempFileIO contents
          |> map Ok
          |> Platform.doAnything doAnything
  Prelude.pure
    Handler
      { eval,
        loadImplicitCradle,
        getCompilerOptions,
        writeTempFile,
        readFile,
        runCpphs
      }

verify :: Handler -> CradleInfo -> Module -> Task Error (List (Example, ExampleResult))
verify handler cradleInfo Module {comments, moduleInfo} =
  withContext handler moduleInfo comments <| \maybeContext comments' ->
    comments'
      |> examples
      |> List.map
        ( \example ->
            verifyExample handler cradleInfo moduleInfo maybeContext example
              |> Task.map (\verified -> (example, ExampleVerifySuccess verified))
              |> Task.onError (\err -> Task.succeed (example, ExampleVerifyFailed err))
        )
      |> Task.parallel

verifyExample ::
  Handler ->
  CradleInfo ->
  ModuleInfo ->
  Maybe Context ->
  Example ->
  Task EvalError Verified
verifyExample Handler {eval} cradleInfo moduleInfo maybeContext example =
  case example of
    VerifiedExample _ code ->
      eval cradleInfo moduleInfo maybeContext (Prelude.unlines code)
    UnverifiedExample _ code ->
      Task.succeed Todo

preloadPaths :: Prelude.IO (List Prelude.FilePath)
preloadPaths =
  Prelude.traverse
    DataPath.getDataFileName
    [ "src/Haskell/Verify/Examples/RunTime.hs",
      "src/Haskell/Verify/Examples/Verified.hs"
    ]

makeImport :: LHE.Syntax.ImportDecl LHE.SrcLoc.SrcSpanInfo -> Hint.ModuleImport
makeImport importDecl =
  Hint.ModuleImport
    { Hint.modName = getModName (LHE.Syntax.importModule importDecl),
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

evalIO ::
  CradleInfo ->
  ModuleInfo ->
  Maybe Context ->
  Prelude.String ->
  Prelude.IO Verified
evalIO CradleInfo {packageDbs, packageIds, languageExtensions, importPaths} moduleInfo maybeContext s = do
  let interpreter =
        case List.map unPackageId packageIds
          ++ List.map unPackageDb packageDbs of
          [] -> Hint.runInterpreter
          args -> Hint.Unsafe.unsafeRunInterpreterWithArgs args
  result <-
    interpreter <| do
      preload <- Hint.lift preloadPaths

      let (unknownLangs, langs) =
            languageExtensions ++ moduleLanguageExtensions moduleInfo
              |> List.map
                ( \(LanguageExtension ex) -> case Text.Read.readMaybe ex of
                    Just lang -> (Nothing, Just lang)
                    Nothing -> (Just ex, Nothing)
                )
              |> List.unzip
              |> Tuple.mapBoth (List.filterMap identity) (List.filterMap identity)
      -- Extensions we can safely ignore.
      let ignoreExts = ["Haskell2010"]
      if List.isEmpty (List.filter (\lang -> not (List.member lang ignoreExts)) unknownLangs)
        then Prelude.pure ()
        else Exception.throwIO (UnkownLanguageExtension unknownLangs)
      let searchPaths = List.map unImportPath importPaths
      Hint.set [Hint.languageExtensions Hint.:= langs, Hint.searchPath Hint.:= searchPaths]

      [ if moduleFilePath moduleInfo == ""
          then []
          else [moduleFilePath moduleInfo],
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
            [ Just "Haskell.Verify.Examples.RunTime",
              Just "Haskell.Verify.Examples.Verified",
              Maybe.map contextModuleName maybeContext
            ]
              |> List.filterMap identity
              |> List.map makeSimpleImport

      Hint.setImportsF (exampleImports ++ imports moduleInfo)
      Hint.interpret s (Hint.as :: Verified)
  case result of
    Prelude.Left err -> Exception.throwIO err
    Prelude.Right ok -> Prelude.pure ok

parse :: Handler -> Prelude.FilePath -> Task Error Module
parse Handler {readFile, runCpphs} path = do
  contents <- readFile path
  let exts = case LHE.readExtensions contents of
        Just (_, exts') -> exts'
        Nothing -> []
  contents' <-
    -- Note: there is implicit magic happening here to preserve line numbers
    -- through preprocessor macros (like #if) runCpphs will drop #line
    -- commands into the output code where necessary which LHE will respect
    -- when generating source positions -}
    if List.member (LHE.EnableExtension LHE.CPP) exts
      then runCpphs path contents
      else Prelude.pure contents
  case LHE.parseFileContentsWithComments (LHE.defaultParseMode {LHE.parseFilename = path}) contents' of
    LHE.Parser.ParseOk ok -> toModule ok
    LHE.Parser.ParseFailed x msg -> Task.fail (ParseFailed x msg)

-- Parses the file for imports / extensions / comments, but also will attempt to find the cradle for project default extensions and module directories
tryLoadImplicitCradle :: Handler -> Prelude.FilePath -> Task Error CradleInfo
tryLoadImplicitCradle handler path =
  Task.onError (\_ -> Task.succeed emptyCradleInfo) <| do
    cradle <- (loadImplicitCradle handler) path
    componentOptions <- (getCompilerOptions handler) path cradle
    let opts = HIE.Bios.Types.componentOptions componentOptions
    Task.succeed
      CradleInfo
        { languageExtensions = List.map LanguageExtension (getDefaultLanguageExtensions opts),
          importPaths = List.map ImportPath (getSearchPaths opts),
          packageDbs = List.map PackageDb (getPackageDbs opts),
          packageIds = List.map PackageId (getPackageIds opts)
        }

examples :: Comment -> List Example
examples comment =
  codeBlocks comment
    |> List.filterMap
      ( \case
          ContextBlock _ _ -> Nothing
          ExampleBlock example -> Just example
      )

contextBlocks :: Comment -> List Prelude.String
contextBlocks comment =
  codeBlocks comment
    |> List.concatMap
      ( \c ->
          case c of
            ContextBlock _ context -> context
            ExampleBlock _ -> []
      )

data Context = Context
  { contextModulePath :: Prelude.FilePath,
    contextModuleName :: Text
  }

withContext :: Handler -> ModuleInfo -> List Comment -> (Maybe Context -> Comment -> Task Error (List a)) -> Task Error (List a)
withContext handler moduleInfo comments go =
  comments
    |> List.indexedMap
      ( \index comments' -> do
          let contextModuleName = "HaskellVerifiedExamplesContext" ++ Text.fromInt index
          case contextBlocks comments' of
            [] -> go Nothing comments'
            xs -> do
              contextModulePath <-
                [ ["module " ++ Text.toList contextModuleName ++ " where"],
                  List.map renderImport (imports moduleInfo),
                  xs
                  ]
                  |> List.concat
                  |> writeTempFile handler
              go (Just Context {contextModulePath, contextModuleName}) comments'
      )
    |> Task.parallel
    |> Task.map List.concat

renderImport :: Hint.ModuleImport -> Prelude.String
renderImport m =
  Prelude.concat
    [ "import ",
      case Hint.modQual m of
        Hint.NotQualified -> Hint.modName m
        Hint.ImportAs q -> Hint.modName m ++ " as " ++ q
        Hint.QualifiedAs Nothing -> "qualified " ++ Hint.modName m
        Hint.QualifiedAs (Just q) -> "qualified " ++ Hint.modName m ++ " as " ++ q,
      case Hint.modImp m of
        Hint.NoImportList -> ""
        Hint.ImportList l -> " (" ++ Data.List.intercalate "," l ++ ")"
        Hint.HidingList l -> " hiding (" ++ Data.List.intercalate "," l ++ ")"
    ]

toModule ::
  ( LHE.Syntax.Module LHE.SrcLoc.SrcSpanInfo,
    List LHE.Comments.Comment
  ) ->
  Task Error Module
toModule parsed =
  case parsed of
    (LHE.Syntax.Module moduleSource moduleHead pragmas imports _, cs) -> do
      let moduleName = case moduleHead of
            (Just (LHE.Syntax.ModuleHead _ (LHE.Syntax.ModuleName _ name) _ _)) -> Just (Text.fromList name)
            Nothing -> Nothing
      let moduleLanguageExtensions = getLanguageExtensions pragmas
      comments <-
        cs
          |> groupBlocks
          |> Prelude.traverse toComment
          |> Result.map List.reverse
          |> taskFromResult
      Task.succeed
        Module
          { moduleInfo =
              ModuleInfo
                { moduleName,
                  moduleSource,
                  moduleLanguageExtensions,
                  imports = List.map makeImport imports
                },
            comments
          }
    _ -> Task.fail UnsupportedModuleType

getLanguageExtensions :: List (LHE.Syntax.ModulePragma l) -> List LanguageExtension
getLanguageExtensions =
  List.concatMap
    ( \case
        LHE.Syntax.LanguagePragma _ ns ->
          List.filterMap
            ( \case
                LHE.Syntax.Ident _ n -> Just (LanguageExtension n)
                _ -> Nothing
            )
            ns
        _ -> []
    )

groupBlocks :: List LHE.Comments.Comment -> List (List LHE.Comments.Comment)
groupBlocks =
  groupWhile
    <| \(LHE.Comments.Comment _ leftSpan _)
        (LHE.Comments.Comment _ rightSpan _) ->
        LHE.SrcLoc.srcSpanEndLine leftSpan + 1
          == LHE.SrcLoc.srcSpanStartLine rightSpan

toComment :: List LHE.Comments.Comment -> Result Error Comment
toComment cs =
  cs
    |> mergeComments [] False
    |> List.filterMap
      ( \(ct, comments) ->
          case ct of
            PlainTextType -> Nothing
            CodeBlockType ->
              comments
                |> List.map (commentValue >> Prelude.dropWhile (/= '>') >> Prelude.drop 2)
                |> toExample (commentsSrcSpan comments)
                |> Result.map ExampleBlock
                |> Just
            HelpTodoType ->
              comments
                |> List.map (commentValue >> Prelude.dropWhile (/= '?') >> Prelude.drop 2)
                |> (\xs -> "evaluteExampleTodo (" : xs ++ [")"])
                |> toExample (commentsSrcSpan comments)
                |> Result.map ExampleBlock
                |> Just
            ContextBlockType ->
              comments
                |> List.map commentValue
                |> Data.List.tail
                |> Data.List.init
                |> List.map (Prelude.drop 1)
                |> ContextBlock (commentsSrcSpan comments)
                |> Ok
                |> Just
      )
    |> combineResults
    |> Result.map Comment

data CommentType
  = CodeBlockType
  | PlainTextType
  | ContextBlockType
  | HelpTodoType
  deriving (Show, Eq)

mergeComments ::
  List (CommentType, List LHE.Comments.Comment) ->
  Bool ->
  List LHE.Comments.Comment ->
  List (CommentType, List LHE.Comments.Comment)
mergeComments acc _ [] = List.reverse acc
mergeComments acc isInContext (next : restNext) =
  let nextCt = commentType next
      stillInContext = if isInContext then nextCt /= ContextBlockType else nextCt == ContextBlockType
      newAcc = case acc of
        [] -> [(nextCt, [next])]
        (prevCt, prev) : restPrev ->
          if isInContext || prevCt == nextCt
            then (prevCt, prev ++ [next]) : restPrev
            else (nextCt, [next]) : acc
   in mergeComments newAcc stillInContext restNext

commentType :: LHE.Comments.Comment -> CommentType
commentType (LHE.Comments.Comment _ _ text) =
  if hasPrefix ">" text
    then CodeBlockType
    else
      if hasPrefix "?" text
        then HelpTodoType
        else
          if hasAt text
            then ContextBlockType
            else PlainTextType

hasAt :: Prelude.String -> Bool
hasAt text = Text.trim (Text.fromList text) == "@"

hasPrefix :: Text -> Prelude.String -> Bool
hasPrefix prefix text =
  Text.startsWith (" " ++ prefix ++ " ") (Text.fromList text)
    || Text.trim (Text.fromList text) == prefix

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

toExample :: LHE.SrcLoc.SrcSpan -> List Prelude.String -> Result Error Example
toExample srcSpan source =
  case LHE.Lexer.lexTokenStream (Prelude.unlines source) of
    LHE.Parser.ParseOk tokens ->
      if Foldable.any ((\sym -> List.member sym knownTokens) << LHE.Lexer.unLoc) tokens
        then Ok (VerifiedExample srcSpan source)
        else Ok (UnverifiedExample srcSpan source)
    LHE.Parser.ParseFailed srcLoc msg ->
      Err (ParseFailed srcLoc msg)

knownTokens :: List LHE.Lexer.Token
knownTokens =
  [ LHE.Lexer.VarSym "==>",
    LHE.Lexer.VarId "evaluteExampleTodo"
  ]

data Reporter
  = Stdout
  | -- | TODO
    Junit
  | LogFile
  deriving (Eq)

report :: List Reporter -> Result Error (List (ModuleInfo, List (Example, ExampleResult))) -> Prelude.IO ()
report reporters result =
  [ if List.member Stdout reporters
      then Just (Reporter.Stdout.report System.IO.stdout result)
      else Nothing
  ]
    |> List.filterMap identity
    |> Async.mapConcurrently_ identity

writeTempFileIO :: List Prelude.String -> Prelude.IO Prelude.FilePath
writeTempFileIO contents = do
  (path, handle) <- System.IO.openTempFile "/tmp" "HaskellVerifiedExamples.hs"
  _ <- Prelude.traverse (System.IO.hPutStrLn handle) contents
  System.IO.hClose handle
  Prelude.pure path

trimPrefix :: Prelude.String -> Prelude.String -> Maybe Prelude.String
trimPrefix prefix text =
  if Data.List.isPrefixOf prefix text
    then Just <| Data.List.drop (Prelude.length prefix) text
    else Nothing

getSearchPaths :: List Prelude.String -> List Prelude.String
getSearchPaths = List.filterMap <| trimPrefix "-i"

getDefaultLanguageExtensions :: List Prelude.String -> List Prelude.String
getDefaultLanguageExtensions = List.filterMap <| trimPrefix "-X"

getPackageDbs :: List Prelude.String -> List Prelude.String
getPackageDbs = getTuples "-package-db"

getPackageIds :: List Prelude.String -> List Prelude.String
getPackageIds = getTuples "-package-id"

getTuples :: Prelude.String -> List Prelude.String -> List Prelude.String
getTuples key options =
  List.concat
    [ [l, r]
      | (l, r) <- Prelude.zip (Data.List.init options) (List.drop 1 options),
        l == key
    ]

taskFromResult :: Result err a -> Task err a
taskFromResult (Err err) = Task.fail err
taskFromResult (Ok a) = Task.succeed a
