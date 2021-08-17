module Main (main) where

import qualified Expect
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
        "comments"
        [ test "returns all comments" <| \() ->
            HVE.comments
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
              |> Expect.equalToContentsOf "test/golden-results/comments.json"
        ]
    ]
