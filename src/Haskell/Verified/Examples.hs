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
verify mod =
  mod
    |> comments
    |> examples
    |> Prelude.traverse (verifyExample (moduleInfo mod))

verifyExample :: ModuleInfo -> Example -> Prelude.IO ExampleResult
verifyExample modInfo example =
  case example of
    VerifiedExample (_, code) -> do
      result <- eval modInfo code
      case result of
        Prelude.Left err ->
          Prelude.pure (ExampleVerifyFailed example err)
        Prelude.Right execResult ->
          ExampleVerifySuccess example execResult
            |> Prelude.pure
    UnverifiedExample (_, code) ->
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

eval :: ModuleInfo -> Text -> Prelude.IO (Prelude.Either Hint.InterpreterError Verified)
eval moduleInfo s = do
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

    Hint.loadModules
      ( if modulePath == ""
          then preload
          else modulePath : preload
      )

    case moduleName moduleInfo of
      Just name -> Hint.setTopLevelModules [Text.toList name]
      Nothing -> Prelude.return ()

    let exampleImports =
          List.map
            makeSimpleImport
            [ "Haskell.Verified.Examples.RunTime",
              "Haskell.Verified.Examples.Verified"
            ]

    Hint.setImportsF (exampleImports ++ imports moduleInfo)
    Hint.interpret (Text.toList s) (Hint.as :: Verified)

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

exampleFromText :: Text -> Example
exampleFromText val =
  toExample LHE.SrcLoc.noSrcSpan val

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
          PlainTextComment _ -> Nothing
          ContextBlockComment _ -> Nothing
          CodeBlockComment example -> Just example
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
    |> List.map
      ( \(ct, LHE.Comments.Comment _ srcSpan val) ->
          case ct of
            CodeBlock ->
              toExample
                (LHE.SrcLoc.noInfoSpan srcSpan)
                (Text.fromList val)
                |> CodeBlockComment
            PlainText -> PlainTextComment (LHE.SrcLoc.noInfoSpan srcSpan, Text.fromList val)
            ContextBlock -> ContextBlockComment (LHE.SrcLoc.noInfoSpan srcSpan, Text.fromList val)
      )

data CommentType = CodeBlock | PlainText | ContextBlock
  deriving (Show)

mergeComments :: List (CommentType, LHE.Comments.Comment) -> Bool -> List LHE.Comments.Comment -> List (CommentType, LHE.Comments.Comment)
mergeComments acc _ [] = List.reverse acc
mergeComments [] _ (next : rest) =
  case commentType next of
    CodeBlock -> mergeComments [(CodeBlock, cleanCodeBlock next)] False rest
    PlainText -> mergeComments [(PlainText, next)] False rest
    ContextBlock -> mergeComments [(ContextBlock, next)] True rest
mergeComments ((_, prevComment) : acc) True (next : rest) =
  case commentType next of
    CodeBlock -> mergeComments ((ContextBlock, concatComment prevComment next) : acc) True rest
    PlainText -> mergeComments ((ContextBlock, concatComment prevComment next) : acc) True rest
    ContextBlock -> mergeComments ((ContextBlock, concatComment prevComment (cleanCodeBlock next)) : acc) False rest
mergeComments (prev@(prevCT, prevComment) : acc) False (next : rest) =
  case (prevCT, commentType next) of
    (CodeBlock, CodeBlock) -> mergeComments ((CodeBlock, concatComment prevComment (cleanCodeBlock next)) : acc) False rest
    (PlainText, PlainText) -> mergeComments ((PlainText, concatComment prevComment next) : acc) False rest
    (PlainText, CodeBlock) -> mergeComments ((CodeBlock, cleanCodeBlock next) : prev : acc) False rest
    (CodeBlock, PlainText) -> mergeComments ((PlainText, next) : prev : acc) False rest
    (_, ContextBlock) -> mergeComments ((ContextBlock, cleanCodeBlock next) : prev : acc) True rest
    (ContextBlock, CodeBlock) -> mergeComments ((CodeBlock, cleanCodeBlock next) : prev : acc) False rest
    (ContextBlock, PlainText) -> mergeComments ((PlainText, next) : prev : acc) False rest

-- @
--
--
--
--
-- @

cleanCodeBlock :: LHE.Comments.Comment -> LHE.Comments.Comment
cleanCodeBlock (LHE.Comments.Comment t s text) =
  text
    |> Prelude.drop 3
    |> LHE.Comments.Comment t s

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
concatComment (LHE.Comments.Comment _ srcSpanA a) (LHE.Comments.Comment _ srcSpanB b) =
  LHE.Comments.Comment True (LHE.SrcLoc.mergeSrcSpan srcSpanA srcSpanB) (a ++ "\n" ++ b)

toExample :: LHE.SrcLoc.SrcSpanInfo -> Text -> Example
toExample srcLocInfo source =
  case LHE.Lexer.lexTokenStream (Text.toList source) of
    LHE.Parser.ParseOk tokens ->
      if Foldable.any ((== LHE.Lexer.VarSym "==>") << LHE.Lexer.unLoc) tokens
        then VerifiedExample (srcLocInfo, source)
        else UnverifiedExample (srcLocInfo, source)
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
