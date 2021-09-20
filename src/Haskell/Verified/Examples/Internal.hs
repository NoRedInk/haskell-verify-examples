module Haskell.Verified.Examples.Internal where

import qualified Control.Concurrent.Async as Async
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

data Module = Module
  { moduleInfo :: ModuleInfo,
    comments :: List Comment
  }
  deriving (Show)

data ModuleInfo = ModuleInfo
  { moduleName :: Maybe Text, -- Headless modules might not have a name
    moduleSource :: LHE.SrcSpanInfo,
    languageExtensions :: List Text,
    imports :: List Hint.ModuleImport,
    importPaths :: List Text,
    packageDbs :: List Text
  }
  deriving (Show)

data Comment
  = CodeBlockComment Example
  | ContextBlockComment (LHE.SrcLoc.SrcSpanInfo, List Prelude.String)
  deriving (Show, Eq)

data Example
  = VerifiedExample (LHE.SrcLoc.SrcSpanInfo, List Prelude.String)
  | UnverifiedExample (LHE.SrcLoc.SrcSpanInfo, List Prelude.String)
  deriving (Show, Eq)

exampleSrcSpan :: Example -> LHE.SrcLoc.SrcSpan
exampleSrcSpan (VerifiedExample (info, _)) = LHE.SrcLoc.srcInfoSpan info
exampleSrcSpan (UnverifiedExample (info, _)) = LHE.SrcLoc.srcInfoSpan info

data ExampleResult
  = ExampleVerifySuccess Example Verified
  | ExampleVerifyFailed Example Hint.InterpreterError
  deriving (Show)

examplesVerified :: List ExampleResult -> Bool
examplesVerified = List.all exampleVerified

exampleVerified :: ExampleResult -> Bool
exampleVerified (ExampleVerifySuccess _ Verified) = True
exampleVerified (ExampleVerifySuccess _ _) = False
exampleVerified (ExampleVerifyFailed _ _) = False

exampleFromResult :: ExampleResult -> Example
exampleFromResult (ExampleVerifySuccess example _) = example
exampleFromResult (ExampleVerifyFailed example _) = example

moduleFilePath :: ModuleInfo -> Prelude.FilePath
moduleFilePath =
  LHE.SrcLoc.srcSpanFilename << LHE.SrcLoc.srcInfoSpan << moduleSource
