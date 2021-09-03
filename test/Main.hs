module Main (main) where

import qualified Expect
import qualified GHC.Stack as Stack
import qualified Haskell.Verified.Examples as HVE
import qualified System.Directory as Directory
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
            result <-
              HVE.parse "test/assets/Simple.hs"
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/parse-simple.hs",
          test "distinguishs examples without `==>`" <| \() -> do
            result <-
              HVE.parse "test/assets/UnverifiedExamples.hs"
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/parse-unverified-examples.hs"
        ],
      describe
        "verify"
        [ test "verfies an example when it succeeds" <| \() -> do
            example <-
              HVE.exampleFromText "1 + 1 ==> 2"
                |> expectJust
            result <-
              example
                |> HVE.verify Nothing [] []
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/verify-verified.hs",
          test "verfies an example when it fails" <| \() -> do
            example <-
              HVE.exampleFromText "1 + 1 ==> 3"
                |> expectJust
            result <-
              example
                |> HVE.verify Nothing [] []
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/verify-unverified.hs",
          test "verfies multiline example (succeeds)" <| \() ->
            do
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
                  |> Text.join "\n"
                  |> HVE.exampleFromText
                  |> expectJust
              result <-
                example
                  |> HVE.verify Nothing ["List"] []
                  |> Expect.fromIO
              result
                |> Debug.toString
                |> Expect.equalToContentsOf "test/golden-results/verify-multiline-verified.hs",
          test "verfies multiline example (fails)" <| \() -> do
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
                |> Text.join "\n"
                |> HVE.exampleFromText
                |> expectJust
            result <-
              example
                |> HVE.verify Nothing ["List"] []
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/verify-multiline-unverified.hs"
        ],
      describe
        "Integration"
        [ test "verifies all examples from a file" <| \() -> do
            assets <-
              Directory.listDirectory "test/assets"
                |> Expect.fromIO
            results <-
              assets
                |> List.map ("test/assets/" ++)
                |> Prelude.traverse
                  ( \modulePath -> do
                      parsed <-
                        HVE.parse modulePath
                          |> Expect.fromIO
                      result <-
                        parsed
                          |> HVE.examples
                          |> Prelude.traverse (HVE.verify (Just modulePath) (List.filterMap Prelude.id [HVE.moduleName parsed]) (HVE.languageExtensions parsed))
                          |> Expect.fromIO
                      Expect.fromResult
                        ( Ok
                            ( modulePath,
                              List.map (Result.map HVE.pretty) result
                            )
                        )
                  )
            results
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/integration-simple.hs"
        ]
    ]

expectJust :: Stack.HasCallStack => Maybe a -> Expect.Expectation' a
expectJust m = do
  case m of
    Just x -> Ok x
    Nothing -> Err "Expected one Just but got Nothing"
    |> Stack.withFrozenCallStack Expect.fromResult
