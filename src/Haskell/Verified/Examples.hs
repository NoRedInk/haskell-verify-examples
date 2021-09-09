module Haskell.Verified.Examples
  ( parse,
    Module (..),
    ModuleInfo (..),
    Example (..),
    examples,
    exampleFromText,
    Comment (..),
    verify,
    pretty,
    makeSimpleImport,
  )
where

import qualified Data.Foldable as Foldable
import qualified HIE.Bios.Cradle
import qualified HIE.Bios.Environment
import qualified HIE.Bios.Flags
import qualified HIE.Bios.Types
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
import qualified Text.Read
import qualified Prelude

data Module = Module
  { moduleInfo :: ModuleInfo,
    comments :: List Comment
  }
  deriving (Show)

data ModuleInfo = ModuleInfo
  { moduleName :: Maybe Text, -- Headless modules might not have a name
    moduleSource :: LHE.SrcSpanInfo,
    languageExtensions :: List Text,
    imports :: List Hint.ModuleImport
  }
  deriving (Show)

data Comment
  = PlainTextComment (LHE.SrcLoc.SrcSpanInfo, Text)
  | CodeBlockComment Example
  deriving (Show, Eq)

data Example
  = VerifiedExample (LHE.SrcLoc.SrcSpanInfo, Text)
  | UnverifiedExample (LHE.SrcLoc.SrcSpanInfo, Text)
  deriving (Show, Eq)

-- TODO imports need to support qualified and stuff. This is just a hack to see how things work so far.
-- We can use setImportsQ.
-- And obviously need to parse it.
--
verify :: Maybe Prelude.FilePath -> ModuleInfo -> Example -> Prelude.IO (Result Text Verified)
verify modulePath ModuleInfo {moduleName, imports, languageExtensions} example =
  case example of
    VerifiedExample (_, code) -> do
      result <- eval modulePath moduleName imports languageExtensions code
      case result of
        Prelude.Left err ->
          let _ = Debug.log "interpret error" err
           in Prelude.pure (Err (Debug.toString err))
        Prelude.Right execResult -> Prelude.pure (Ok execResult)
    UnverifiedExample (_, code) ->
      code
        |> Text.toList
        |> NoExampleResult
        |> Ok
        |> Prelude.pure

preloadPaths :: Prelude.IO (List Prelude.FilePath)
preloadPaths = Prelude.traverse DataPath.getDataFileName paths
  where
    paths =
      [ "src/Haskell/Verified/Examples/RunTime.hs",
        "src/Haskell/Verified/Examples/Verified.hs"
      ]

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

eval :: Maybe Prelude.FilePath -> Maybe Text -> List Hint.ModuleImport -> List Text -> Text -> Prelude.IO (Prelude.Either Hint.InterpreterError Verified)
eval modulePath moduleName imports extensions s = do
  maybeFlags <- case modulePath of
    Nothing -> Prelude.pure Nothing
    Just path -> do
      cradle <- HIE.Bios.Cradle.loadImplicitCradle path
      cradleResult <- HIE.Bios.Flags.getCompilerOptions path cradle
      case cradleResult of
        HIE.Bios.Types.CradleSuccess r -> Prelude.pure (Just r)
        err ->
          let _ = Debug.log "err" err
           in Debug.todo "TODO cradle failure"
  let componentOptions = case maybeFlags of
        Nothing -> []
        Just flags -> List.map Text.fromList <| HIE.Bios.Types.componentOptions flags

  let interpreter = case maybeFlags of
        Nothing -> Hint.runInterpreter
        Just flags -> Hint.Unsafe.unsafeRunInterpreterWithArgs <| List.map Text.toList <| getPackageDbs componentOptions
  interpreter <| do
    preload <- Hint.lift preloadPaths

    -- TODO: Throw nice "unrecognized extension" error instead of ignoring here
    let langs = List.filterMap (\ex -> Text.Read.readMaybe <| Text.toList ex) (getDefaultLanguageExtensions componentOptions ++ extensions)
    let searchPaths = List.map Text.toList <| getSearchPaths componentOptions
    Hint.set [Hint.languageExtensions Hint.:= langs, Hint.searchPath Hint.:= searchPaths]

    Hint.loadModules
      ( case modulePath of
          Just path -> path : preload
          Nothing -> preload
      )

    case moduleName of
      Just name -> Hint.setTopLevelModules [Text.toList name]
      Nothing -> Prelude.return ()

    let exampleImports =
          List.map
            makeSimpleImport
            [ "Haskell.Verified.Examples.RunTime",
              "Haskell.Verified.Examples.Verified"
            ]

    Hint.setImportsF (exampleImports ++ imports)
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

examples :: List Comment -> List Example
examples =
  List.filterMap
    ( \c ->
        case c of
          PlainTextComment _ -> Nothing
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
                    imports = List.map makeImport imports
                  },
              comments = toComments cs
            }
    _ ->
      Debug.todo "TODO unsupported module type"

toComments :: List LHE.Comments.Comment -> List Comment
toComments cs =
  cs
    |> mergeComments []
    |> List.map
      ( \(ct, LHE.Comments.Comment _ srcSpan val) ->
          case ct of
            CodeBlock ->
              toExample
                (LHE.SrcLoc.noInfoSpan srcSpan)
                (Text.fromList val)
                |> CodeBlockComment
            PlainText -> PlainTextComment (LHE.SrcLoc.noInfoSpan srcSpan, Text.fromList val)
      )

data CommentType = CodeBlock | PlainText
  deriving (Show)

mergeComments :: List (CommentType, LHE.Comments.Comment) -> List LHE.Comments.Comment -> List (CommentType, LHE.Comments.Comment)
mergeComments acc [] = List.reverse acc
mergeComments [] (next : rest) =
  mergeComments
    [ case commentType next of
        CodeBlock -> (CodeBlock, cleanCodeBlock next)
        PlainText -> (PlainText, next)
    ]
    rest
mergeComments (prev@(prevCT, prevComment) : acc) (next : rest) =
  mergeComments
    ( case (prevCT, commentType next) of
        (CodeBlock, CodeBlock) -> (CodeBlock, concatComment prevComment (cleanCodeBlock next)) : acc
        (PlainText, PlainText) -> (PlainText, concatComment prevComment next) : acc
        (PlainText, CodeBlock) -> (CodeBlock, cleanCodeBlock next) : prev : acc
        (CodeBlock, PlainText) -> (PlainText, next) : prev : acc
    )
    rest

cleanCodeBlock :: LHE.Comments.Comment -> LHE.Comments.Comment
cleanCodeBlock (LHE.Comments.Comment t s text) =
  text
    |> Prelude.drop 3
    |> LHE.Comments.Comment t s

commentType :: LHE.Comments.Comment -> CommentType
commentType (LHE.Comments.Comment _ _ text) =
  if Text.startsWith " > " (Text.fromList text)
    || Text.trim (Text.fromList text) == ">"
    then CodeBlock
    else PlainText

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

pretty :: Verified -> List Text
pretty verified =
  case verified of
    Verified -> []
    Unverified expected actual ->
      [ "The example was incorrect and couldn't be verified.",
        "",
        "We expected:",
        Text.fromList expected,
        "",
        "but received",
        Text.fromList actual
      ]
    NoExampleResult example ->
      [ "No example result was provided. For example:",
        "",
        Text.fromList example
      ]
