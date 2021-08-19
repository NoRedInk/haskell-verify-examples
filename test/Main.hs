module Main (main) where

import qualified Expect
import qualified GHC.Stack as Stack
import qualified Haskell.Verified.Examples as HVE
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
              |> Expect.equalToContentsOf "test/golden-results/parseSimple.hs",
          test "distinguishs examples without `==>`" <| \() -> do
            result <-
              HVE.parse "test/assets/UnverifiedExamples.hs"
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/parseUnverifiedExamples.hs"
        ],
      describe
        "run"
        [ test "runs an example when it succeeds" <| \() -> do
            example <-
              HVE.exampleFromText "1 + 1 ==> 2"
                |> expectJust
            result <-
              example
                |> HVE.run []
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/runVerified.hs",
          test "runs an example when it fails" <| \() -> do
            example <-
              HVE.exampleFromText "1 + 1 ==> 3"
                |> expectJust
            result <-
              example
                |> HVE.run []
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/runUnverified.hs",
          test "runs multiline example (succeeds)" <| \() ->
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
                  |> HVE.run ["List"]
                  |> Expect.fromIO
              result
                |> Debug.toString
                |> Expect.equalToContentsOf "test/golden-results/runMultilineVerified.hs",
          test "runs multiline example (fails)" <| \() -> do
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
                |> HVE.run ["List"]
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/runMultilineUnverified.hs"
        ]
    ]

expectJust :: Stack.HasCallStack => Maybe a -> Expect.Expectation' a
expectJust m = do
  case m of
    Just x -> Ok x
    Nothing -> Err "Expected one Just but got Nothing"
    |> Stack.withFrozenCallStack Expect.fromResult
