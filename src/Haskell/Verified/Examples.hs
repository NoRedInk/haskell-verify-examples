module Haskell.Verified.Examples
  ( parse,
    ModuleWithExamples (..),
    ModuleInfo (..),
    Example (..),
    exampleFromText,
    Comment (..),
    verify,
    pretty,
    makeSimpleImport,
  )
where

import qualified Data.Foldable as Foldable
import Haskell.Verified.Examples.Verified (Verified (..))
import qualified Language.Haskell.Exts as LHE
import qualified Language.Haskell.Exts.Comments as LHE.Comments
import qualified Language.Haskell.Exts.Lexer as LHE.Lexer
import qualified Language.Haskell.Exts.Parser as LHE.Parser
import Language.Haskell.Exts.SrcLoc ((<++>))
import qualified Language.Haskell.Exts.SrcLoc as LHE.SrcLoc
import qualified Language.Haskell.Exts.Syntax as LHE.Syntax
import qualified Language.Haskell.Interpreter as Hint
import NriPrelude
import qualified Paths_haskell_verified_examples as DataPath
import qualified Text.Read
import qualified Prelude

data ModuleWithExamples = ModuleWithExamples
  { moduleInfo :: ModuleInfo,
    comments :: List Comment,
    examples :: List Example
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
  = Comment (LHE.SrcLoc.SrcSpanInfo, Text)
  | ExampleComment (LHE.SrcLoc.SrcSpanInfo, Text)
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
    getName (LHE.Syntax.Symbol _ n) = n
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
eval modulePath moduleName imports extensions s =
  Hint.runInterpreter <| do
    preload <- Hint.lift preloadPaths

    -- TODO: Throw nice "unrecognized extension" error instead of ignoring here
    let langs = List.filterMap (\ex -> Text.Read.readMaybe <| Text.toList ex) extensions
    Hint.set [Hint.languageExtensions Hint.:= langs]

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

exampleFromText :: Text -> Maybe Example
exampleFromText val =
  ExampleComment (LHE.SrcLoc.noSrcSpan, val)
    |> toExamples

parse :: Prelude.FilePath -> Prelude.IO ModuleWithExamples
parse path = do
  parsed <- parseFileWithComments path
  case parsed of
    LHE.Parser.ParseOk ok -> Prelude.pure (toModuleWithExamples ok)
    LHE.Parser.ParseFailed x msg ->
      Debug.todo (Debug.toString x ++ Debug.toString msg)

toModuleWithExamples ::
  ( LHE.Syntax.Module LHE.SrcLoc.SrcSpanInfo,
    List LHE.Comments.Comment
  ) ->
  ModuleWithExamples
toModuleWithExamples parsed =
  case parsed of
    (LHE.Syntax.Module moduleSource moduleHead pragmas imports _, cs) ->
      let moduleName = case moduleHead of
            (Just (LHE.Syntax.ModuleHead _ (LHE.Syntax.ModuleName _ name) _ _)) -> Just <| Text.fromList name
            Nothing -> Nothing
          comments = toComments cs
          examples = List.filterMap toExamples comments
          languageExtensions = [Text.fromList n | LHE.Syntax.LanguagePragma _ ns <- pragmas, (LHE.Syntax.Ident _ n) <- ns]
       in ModuleWithExamples
            { moduleInfo =
                ModuleInfo
                  { moduleName,
                    moduleSource,
                    languageExtensions,
                    imports = List.map makeImport imports
                  },
              comments,
              examples
            }
    _ ->
      Debug.todo "TODO unsupported module type"

toComments :: List LHE.Comments.Comment -> List Comment
toComments cs =
  cs
    |> List.map
      ( \(LHE.Comments.Comment _ srcSpan val) ->
          let val_ = Text.fromList val
           in if Text.startsWith " > " val_
                then ExampleComment (LHE.SrcLoc.noInfoSpan srcSpan, Text.dropLeft 3 val_)
                else Comment (LHE.SrcLoc.noInfoSpan srcSpan, val_)
      )
    |> mergeComments

toExamples :: Comment -> Maybe Example
toExamples (Comment _) = Nothing
toExamples (ExampleComment (srcLocInfo, source)) =
  case LHE.Lexer.lexTokenStream (Text.toList source) of
    LHE.Parser.ParseOk tokens ->
      let verified = Foldable.any ((== LHE.Lexer.VarSym "==>") << LHE.Lexer.unLoc) tokens
       in Just
            <| if verified
              then VerifiedExample (srcLocInfo, source)
              else UnverifiedExample (srcLocInfo, source)
    LHE.Parser.ParseFailed _ msg ->
      let _ = Debug.log "msg" msg
       in Debug.todo "TODO"

mergeComments :: List Comment -> List Comment
mergeComments cs =
  List.foldl
    ( \c xs ->
        case (c, xs) of
          (_, []) -> [c]
          (Comment (x, y), Comment (px, py) : rest) ->
            Comment (x <++> px, py ++ "\n" ++ y) : rest
          (ExampleComment (x, y), ExampleComment (px, py) : rest) ->
            ExampleComment (x <++> px, py ++ "\n" ++ y) : rest
          (c', prev : rest) ->
            c' : prev : rest
    )
    []
    cs
    |> List.reverse

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
