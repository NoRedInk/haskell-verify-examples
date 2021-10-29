module Main (main) where

import qualified Control.Concurrent.Async as Async
import qualified Data.Text.IO
import qualified Expect
import qualified Haskell.Verify.Examples as HVE
import qualified Haskell.Verify.Examples.Internal as Internal
import qualified Haskell.Verify.Examples.Reporter.Stdout as Reporter.Stdout
import qualified Language.Haskell.Exts.SrcLoc as LHE.SrcLoc
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO
import Test (Test, describe, test)
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = do
  assets <- Directory.listDirectory "test/assets/"
  Test.run (tests assets)

tests :: List Prelude.FilePath -> Test
tests assets =
  describe
    "Haskell.Verify.Examples"
    [ describe
        "parse"
        [ test "returns all comments" <| \() -> do
            handler <- Expect.fromIO (Platform.silentHandler >>= HVE.handler)
            result <-
              HVE.parse handler "test/assets/Simple.hs"
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/parse-simple.hs",
          test "distinguishs examples without `==>`" <| \() -> do
            handler <- Expect.fromIO (Platform.silentHandler >>= HVE.handler)
            result <-
              HVE.parse handler "test/assets/UnverifiedExamples.hs"
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/parse-unverified-examples.hs",
          test "parses context code" <| \() -> do
            handler <- Expect.fromIO (Platform.silentHandler >>= HVE.handler)
            result <-
              HVE.parse handler "test/assets/WithContext.hs"
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/parse-with-context.hs"
        ],
      describe
        "Integration"
        ( List.map
            ( \modulePath ->
                test ("verifies all examples from a file from " ++ Text.fromList modulePath) <| \() -> do
                  handler <- Expect.fromIO (Platform.silentHandler >>= HVE.handler)
                  results <-
                    Expect.succeeds <| do
                      parsed <- HVE.parse handler ("test/assets/" ++ modulePath)
                      result <- HVE.verify handler Internal.emptyCradleInfo parsed Internal.ShowTodos
                      Task.succeed (HVE.moduleInfo parsed, result)
                  contents <-
                    withTempFile (\handle -> Reporter.Stdout.report handle (Ok [results]))
                  contents
                    |> Expect.equalToContentsOf ("test/golden-results/integration-" ++ Text.fromList modulePath ++ ".hs")
            )
            assets
        )
    ]

-- | Provide a temporary file for a test to do some work in, then return the
-- contents of the file when the test is done with it.
withTempFile :: (System.IO.Handle -> Prelude.IO ()) -> Expect.Expectation' Text
withTempFile go =
  Expect.fromIO <| do
    (path, handle) <-
      System.IO.openTempFile "/tmp" "nri-haskell-libraries-test-file"
    go handle
    System.IO.hClose handle
    Data.Text.IO.readFile path
