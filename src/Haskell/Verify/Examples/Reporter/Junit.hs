{-# LANGUAGE NumericUnderscores #-}

-- | Module for presenting test results as a Junit XML file.
--
-- Lifted in large part from: https://github.com/stoeffel/tasty-test-reporter
module Haskell.Verify.Examples.Reporter.Junit
  ( report,
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Data.ByteString as BS
import qualified Data.Text
import qualified Data.Text.Encoding as TE
import qualified GHC.Stack as Stack
import Haskell.Verify.Examples.Internal
import Haskell.Verify.Examples.Verified (Verified (..))
import qualified List
import NriPrelude
import qualified Platform
import qualified System.Directory as Directory
import System.FilePath (FilePath)
import qualified System.FilePath as FilePath
import qualified Text
import qualified Text.Colour
import qualified Text.XML.JUnit as JUnit
import qualified Tuple
import qualified Prelude

report :: FilePath -> Result Error (List (ModuleInfo, List (Example, ExampleResult))) -> Prelude.IO ()
report path result = do
  createPathDirIfMissing path
  results <- testResults result
  JUnit.writeXmlReport path results

testResults :: Result Error (List (ModuleInfo, List (Example, ExampleResult))) -> Prelude.IO (List JUnit.TestSuite)
testResults result =
  case result of
    Ok modules ->
      List.concatMap renderExamples modules
        |> Prelude.pure
    Err err -> Debug.todo ""

renderExamples :: (ModuleInfo, List (Example, ExampleResult)) -> List JUnit.TestSuite
renderExamples (moduleInfo, examples) =
  List.map (renderExample moduleInfo) examples

renderExample :: ModuleInfo -> (Example, ExampleResult) -> JUnit.TestSuite
renderExample moduleInfo (example, result) =
  case example of
    VerifiedExample _ contents ->
      case result of
        ExampleVerifySuccess _ ->
          JUnit.passed (Text.join "\n" <| List.map Text.fromList contents)
            |> inSuite
        ExampleVerifyFailed evalFailed ->
          JUnit.failed (Debug.toString evalFailed)
            |> inSuite
    UnverifiedExample _ contents ->
      JUnit.passed (Text.join "\n" <| List.map Text.fromList contents)
        |> inSuite
  where
    inSuite = JUnit.inSuite (Maybe.withDefault "No module name" (moduleName moduleInfo))

-- suiteName :: Internal.SingleTest a -> Text
-- suiteName test =
--   Internal.describes test
--     |> Text.join " - "
--
-- stackFrame :: Internal.SingleTest a -> Maybe Text
-- stackFrame test =
--   Internal.loc test
--     |> map
--       ( \loc ->
--           Data.Text.pack
--             ( Stack.srcLocFile loc
--                 ++ ":"
--                 ++ Prelude.show (Stack.srcLocStartLine loc)
--             )
--       )
--
-- duration :: Platform.TracingSpan -> Float
-- duration test =
--   let duration' = Platform.finished test - Platform.started test
--    in Prelude.fromIntegral (Platform.inMicroseconds duration') / 1000_000
--
createPathDirIfMissing :: FilePath.FilePath -> Prelude.IO ()
createPathDirIfMissing path = do
  dirPath <- map FilePath.takeDirectory (Directory.canonicalizePath path)
  Directory.createDirectoryIfMissing True dirPath
