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
            HVE.comments "-- hello world"
              |> Expect.equal
                [ HVE.Comment "-- hello world"
                ]
        ]
    ]
