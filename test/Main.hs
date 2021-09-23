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
            handler <- Expect.fromIO HVE.handler
            result <-
              HVE.parse handler "test/assets/Simple.hs"
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/parse-simple.hs",
          test "distinguishs examples without `==>`" <| \() -> do
            handler <- Expect.fromIO HVE.handler
            result <-
              HVE.parse handler "test/assets/UnverifiedExamples.hs"
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/parse-unverified-examples.hs",
          test "parses context code" <| \() -> do
            handler <- Expect.fromIO HVE.handler
            result <-
              HVE.parse handler "test/assets/WithContext.hs"
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/parse-with-context.hs"
        ],
      describe
        "verifyExample"
        [ test "verfies an example when it succeeds" <| \() -> do
            example <- Expect.fromResult (HVE.exampleFromText "1 + 1 ==> 2")
            handler <- Expect.fromIO HVE.handler
            result <-
              example
                |> HVE.verifyExample
                  handler
                  (HVE.shimModuleWithImports ["NriPrelude"])
                  Nothing
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/verifyExample-verified.hs",
          test "verfies an example when it fails" <| \() -> do
            example <- Expect.fromResult (HVE.exampleFromText "1 + 1 ==> 3")
            handler <- Expect.fromIO HVE.handler
            result <-
              example
                |> HVE.verifyExample
                  handler
                  (HVE.shimModuleWithImports ["NriPrelude"])
                  Nothing
                |> Expect.succeeds
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/verifyExample-unverified.hs",
          test "verfies multiline example (succeeds)" <| \() ->
            do
              handler <- Expect.fromIO HVE.handler
              example <-
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
                  |> Expect.fromResult
              result <-
                example
                  |> HVE.verifyExample
                    handler
                    (HVE.shimModuleWithImports ["List", "NriPrelude"])
                    Nothing
                  |> Expect.succeeds
              result
                |> Debug.toString
                |> Expect.equalToContentsOf "test/golden-results/verifyExample-multiline-verified.hs",
          test "verfies multiline example (fails)" <| \() -> do
            handler <- Expect.fromIO HVE.handler
            example <-
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
                |> Expect.fromResult
            result <-
              example
                |> HVE.verifyExample
                  handler
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
            handler <- Expect.fromIO HVE.handler
            assets <-
              Directory.listDirectory "test/assets"
                |> Expect.fromIO
            results <-
              assets
                |> List.filter (Text.fromList >> Text.startsWith "Fails" >> not)
                |> List.map ("test/assets/" ++)
                |> List.map
                  ( \modulePath -> do
                      parsed <- HVE.parse handler modulePath
                      result <- HVE.verify handler parsed
                      Task.succeed (HVE.moduleInfo parsed, result)
                  )
                |> Task.parallel
                |> Expect.succeeds
            contents <-
              withTempFile (\handle -> Reporter.Stdout.report handle results)
            contents
              |> Expect.equalToContentsOf "test/golden-results/integration-simple.hs",
          test "failing examples" <| \() -> do
            handler <- Expect.fromIO HVE.handler
            assets <-
              Directory.listDirectory "test/assets"
                |> Expect.fromIO
            errors <-
              assets
                |> List.filter (Text.fromList >> Text.startsWith "Fails")
                |> List.map ("test/assets/" ++)
                |> List.map
                  ( \modulePath -> do
                      parsed <- HVE.parse handler modulePath
                      result <- HVE.verify handler parsed
                      Task.succeed ()
                  )
                |> Task.parallel
                |> Expect.fails
            contents <-
              withTempFile (\handle -> Reporter.Stdout.reportError handle errors)
            contents
              |> Expect.equalToContentsOf "test/golden-results/integration-fails.hs"
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
