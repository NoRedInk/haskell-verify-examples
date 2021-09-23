module Main (main) where

import qualified Control.Concurrent.Async as Async
import qualified Data.Text.IO
import qualified Expect
import qualified Haskell.Verified.Examples as HVE
import qualified Haskell.Verified.Examples.Reporter.Stdout as Reporter.Stdout
import qualified Language.Haskell.Exts.SrcLoc as LHE.SrcLoc
import qualified System.Directory as Directory
import qualified System.IO
import Test (Test, describe, test)
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = Test.run tests

tests :: Test
tests =
  describe
    "Haskell.Verified.Examples"
    [ describe
        "parse"
        [ test "returns all comments" <| \() -> do
            doAnything <- Expect.fromIO Platform.doAnythingHandler
            result <-
              HVE.parse doAnything "test/assets/Simple.hs"
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/parse-simple.hs",
          test "distinguishs examples without `==>`" <| \() -> do
            doAnything <- Expect.fromIO Platform.doAnythingHandler
            result <-
              HVE.parse doAnything "test/assets/UnverifiedExamples.hs"
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/parse-unverified-examples.hs",
          test "parses context code" <| \() -> do
            doAnything <- Expect.fromIO Platform.doAnythingHandler
            result <-
              HVE.parse doAnything "test/assets/WithContext.hs"
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/parse-with-context.hs"
        ],
      describe
        "verifyExample"
        [ test "verfies an example when it succeeds" <| \() -> do
            let example = HVE.exampleFromText "1 + 1 ==> 2"
            doAnything <- Expect.fromIO Platform.doAnythingHandler
            result <-
              example
                |> HVE.verifyExample
                  doAnything
                  (HVE.shimModuleWithImports ["NriPrelude"])
                  Nothing
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/verifyExample-verified.hs",
          test "verfies an example when it fails" <| \() -> do
            let example = HVE.exampleFromText "1 + 1 ==> 3"
            doAnything <- Expect.fromIO Platform.doAnythingHandler
            result <-
              example
                |> HVE.verifyExample
                  doAnything
                  (HVE.shimModuleWithImports ["NriPrelude"])
                  Nothing
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/verifyExample-unverified.hs",
          test "verfies multiline example (succeeds)" <| \() ->
            do
              doAnything <- Expect.fromIO Platform.doAnythingHandler
              let example =
                    [ "[ 1",
                      ", 2",
                      ", 3",
                      "]",
                      "|> List.map (+ 1)",
                      "==>",
                      "[ 2",
                      ", 3",
                      ", 4",
                      "]"
                    ]
                      |> Prelude.unlines
                      |> HVE.exampleFromText
              result <-
                example
                  |> HVE.verifyExample
                    doAnything
                    (HVE.shimModuleWithImports ["List", "NriPrelude"])
                    Nothing
                  |> Expect.succeeds
              result
                |> Debug.toString
                |> Expect.equalToContentsOf "test/golden-results/verifyExample-multiline-verified.hs",
          test "verfies multiline example (fails)" <| \() -> do
            doAnything <- Expect.fromIO Platform.doAnythingHandler
            let example =
                  [ "[ 1",
                    ", 2",
                    ", 3",
                    "]",
                    "|> List.map (+ 1)",
                    "==>",
                    "[ 2",
                    ", 3",
                    ", 5",
                    "]"
                  ]
                    |> Prelude.unlines
                    |> HVE.exampleFromText
            result <-
              example
                |> HVE.verifyExample
                  doAnything
                  (HVE.shimModuleWithImports ["List", "NriPrelude"])
                  Nothing
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/verifyExample-multiline-unverified.hs"
        ],
      describe
        "Integration"
        [ test "verifies all examples from a file" <| \() -> do
            doAnything <- Expect.fromIO Platform.doAnythingHandler
            assets <-
              Directory.listDirectory "test/assets"
                |> Expect.fromIO
            results <-
              assets
                |> List.map ("test/assets/" ++)
                |> List.map
                  ( \modulePath -> do
                      parsed <- HVE.parse doAnything modulePath
                      result <- HVE.verify doAnything parsed
                      Task.succeed (HVE.moduleInfo parsed, result)
                  )
                |> Task.parallel
                |> Expect.succeeds
            contents <-
              withTempFile (\handle -> Reporter.Stdout.report handle results)
            contents
              |> Expect.equalToContentsOf "test/golden-results/integration-simple.hs"
        ]
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
