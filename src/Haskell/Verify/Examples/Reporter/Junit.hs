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
    Ok passed ->
      List.concatMap renderPassed passed
        |> Prelude.pure
    Err err -> Debug.todo ""

renderPassed :: (ModuleInfo, List (Example, ExampleResult)) -> List JUnit.TestSuite
renderPassed (moduleInfo, examples) =
  examples
    |> List.map
      ( \(example, result) ->
          JUnit.inSuite (Maybe.withDefault "" (moduleName moduleInfo))
            <| case example of
              VerifiedExample _ contents ->
                JUnit.passed (Text.join "\n" <| List.map Text.fromList contents)
              UnverifiedExample _ contents ->
                JUnit.passed (Text.join "\n" <| List.map Text.fromList contents)
      )

--
--
-- renderSkipped :: Internal.SingleTest Internal.NotRan -> JUnit.TestSuite
-- renderSkipped test =
--   JUnit.skipped (Internal.name test)
--     |> JUnit.inSuite (suiteName test)
--
-- renderFailed ::
--   Internal.SingleTest (Platform.TracingSpan, Internal.Failure) ->
--   Maybe (Stack.SrcLoc, BS.ByteString) ->
--   JUnit.TestSuite
-- renderFailed test maybeSrcLoc =
--   case Internal.body test of
--     (tracingSpan, Internal.FailedAssertion msg _) ->
--       let msg' = case maybeSrcLoc of
--             Nothing -> msg
--             Just (loc, src) ->
--               Test.Reporter.Internal.renderSrcLoc loc src
--                 |> Text.Colour.renderChunksBS Text.Colour.WithoutColours
--                 |> TE.decodeUtf8
--                 |> (\srcStr -> srcStr ++ "\n" ++ msg)
--        in JUnit.failed (Internal.name test)
--             |> JUnit.stderr msg'
--             |> ( case stackFrame test of
--                    Nothing -> identity
--                    Just frame -> JUnit.failureStackTrace [frame]
--                )
--             |> JUnit.time (duration tracingSpan)
--             |> JUnit.inSuite (suiteName test)
--     (tracingSpan, Internal.ThrewException err) ->
--       JUnit.errored (Internal.name test)
--         |> JUnit.errorMessage "This test threw an exception."
--         |> JUnit.stderr (Data.Text.pack (Exception.displayException err))
--         |> ( case stackFrame test of
--                Nothing -> identity
--                Just frame -> JUnit.errorStackTrace [frame]
--            )
--         |> JUnit.time (duration tracingSpan)
--         |> JUnit.inSuite (suiteName test)
--     (tracingSpan, Internal.TookTooLong) ->
--       JUnit.errored (Internal.name test)
--         |> JUnit.errorMessage "This test timed out."
--         |> ( case stackFrame test of
--                Nothing -> identity
--                Just frame -> JUnit.errorStackTrace [frame]
--            )
--         |> JUnit.time (duration tracingSpan)
--         |> JUnit.inSuite (suiteName test)
--     (tracingSpan, Internal.TestRunnerMessedUp msg) ->
--       JUnit.errored (Internal.name test)
--         |> JUnit.errorMessage
--           ( Text.join
--               "\n"
--               [ "Test runner encountered an unexpected error:",
--                 msg,
--                 "",
--                 "This is a bug.",
--                 "If you have some time to report the bug it would be much appreciated!",
--                 "You can do so here: https://github.com/NoRedInk/haskell-libraries/issues"
--               ]
--           )
--         |> ( case stackFrame test of
--                Nothing -> identity
--                Just frame -> JUnit.errorStackTrace [frame]
--            )
--         |> JUnit.time (duration tracingSpan)
--         |> JUnit.inSuite (suiteName test)
--
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
