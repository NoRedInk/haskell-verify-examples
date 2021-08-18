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
        "moduleWithExamples"
        [ test "returns all comments" <| \() ->
            HVE.moduleWithExamples
              ( Text.join
                  "\n"
                  [ "module Foo where",
                    "-- hello world",
                    "--",
                    "-- > test",
                    "-- > ==> 1",
                    "--",
                    "-- > test + test",
                    "-- > ==> 2",
                    "test = 1"
                  ]
              )
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/moduleWithExamples.hs",
          test "distinguishs examples without `==>`" <| \() ->
            HVE.moduleWithExamples
              ( Text.join
                  "\n"
                  [ "module Foo where",
                    "-- hello world",
                    "--",
                    "-- > test",
                    "-- > ==> 1",
                    "--",
                    "-- > test",
                    "--",
                    "-- > test + test",
                    "-- > ==> 2",
                    "test = 1"
                  ]
              )
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/moduleWithUnverifiedExamples.hs"
        ],
      describe
        "run"
        [ test "runs an example when it succeeds" <| \() -> do
            example <-
              HVE.exampleFromText "1 + 1 ==> 2"
                |> expectJust
            result <-
              example
                |> HVE.run
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/run-succeeds.hs",
          test "runs an example when it fails" <| \() -> do
            example <-
              HVE.exampleFromText "1 + 1 ==> 3"
                |> expectJust
            result <-
              example
                |> HVE.run
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/run-fails.hs"
        ]
    ]

expectJust :: Stack.HasCallStack => Maybe a -> Expect.Expectation' a
expectJust m = do
  case m of
    Just x -> Ok x
    Nothing -> Err "Expected one Just but got Nothing"
    |> Stack.withFrozenCallStack Expect.fromResult
