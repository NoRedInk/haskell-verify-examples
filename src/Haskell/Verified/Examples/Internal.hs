module Haskell.Verified.Examples.Internal where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception.Safe as Exception
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
import qualified System.IO
import qualified Text.Read
import qualified Prelude

data Error
  = ParseFailed LHE.SrcLoc.SrcLoc Prelude.String
  | CradleFailed HIE.Bios.Types.CradleError
  | UnsupportedModuleType
  | EvalFailed EvalError
  | ReadFileFailed Prelude.IOError
  deriving (Show)

data EvalError
  = UnkownLanguageExtension (List Prelude.String)
  | InterpreterError Hint.InterpreterError
  deriving (Show)

instance Exception.Exception EvalError

data Module = Module
  { moduleInfo :: ModuleInfo,
    comments :: List Comment
  }
  deriving (Show)

newtype Comment = Comment {codeBlocks :: List CodeBlock} deriving (Show)

data ModuleInfo = ModuleInfo
  { moduleName :: Maybe Text, -- Headless modules might not have a name
    moduleSource :: LHE.SrcSpanInfo,
    imports :: List Hint.ModuleImport,
    moduleLanguageExtensions :: List LanguageExtension
  }
  deriving (Show)

data CradleInfo = CradleInfo
  { languageExtensions :: List LanguageExtension,
    importPaths :: List ImportPath,
    packageDbs :: List PackageDb
  }

emptyCradleInfo :: CradleInfo
emptyCradleInfo = CradleInfo [] [] []

newtype LanguageExtension = LanguageExtension
  {unLanguageExtension :: Prelude.String}
  deriving (Show)

newtype ImportPath = ImportPath
  {unImportPath :: Prelude.String}
  deriving (Show)

newtype PackageDb = PackageDb
  {unPackageDb :: Prelude.String}
  deriving (Show)

data CodeBlock
  = ExampleBlock Example
  | ContextBlock LHE.SrcLoc.SrcSpan (List Prelude.String)
  deriving (Show, Eq)

data Example
  = VerifiedExample LHE.SrcLoc.SrcSpan (List Prelude.String)
  | UnverifiedExample LHE.SrcLoc.SrcSpan (List Prelude.String)
  deriving (Show, Eq)

exampleSrcSpan :: Example -> LHE.SrcLoc.SrcSpan
exampleSrcSpan (VerifiedExample span _) = span
exampleSrcSpan (UnverifiedExample span _) = span

data ExampleResult
  = ExampleVerifySuccess Verified
  | ExampleVerifyFailed EvalError
  deriving (Show)

examplesVerified :: List ExampleResult -> Bool
examplesVerified = List.all exampleVerified

exampleVerified :: ExampleResult -> Bool
exampleVerified (ExampleVerifySuccess Verified) = True
exampleVerified (ExampleVerifySuccess _) = False
exampleVerified (ExampleVerifyFailed _) = False

moduleFilePath :: ModuleInfo -> Prelude.FilePath
moduleFilePath =
  LHE.SrcLoc.srcSpanFilename << LHE.SrcLoc.srcInfoSpan << moduleSource

combineResults :: List (Result x a) -> Result x (List a)
combineResults = List.foldr (Result.map2 (:)) (Ok [])

shimModuleWithImports :: List Text -> ModuleInfo
shimModuleWithImports imports =
  ModuleInfo
    { moduleName = Nothing,
      moduleSource = LHE.SrcLoc.noSrcSpan,
      imports = List.map makeSimpleImport imports,
      moduleLanguageExtensions = []
    }

makeSimpleImport :: Text -> Hint.ModuleImport
makeSimpleImport name = Hint.ModuleImport (Text.toList name) Hint.NotQualified Hint.NoImportList
