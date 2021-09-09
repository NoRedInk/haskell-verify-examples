module Main (main) where

import qualified Expect
import qualified Haskell.Verified.Examples as HVE
import qualified Language.Haskell.Exts.SrcLoc as LHE.SrcLoc
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
            let example = HVE.exampleFromText "1 + 1 ==> 2"
            result <-
              example
                |> HVE.verify
                  Nothing
                  HVE.ModuleInfo
                    { HVE.moduleName = Nothing,
                      HVE.moduleSource = LHE.SrcLoc.noSrcSpan,
                      HVE.imports = [HVE.makeSimpleImport "NriPrelude"],
                      HVE.languageExtensions = []
                    }
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/verify-verified.hs",
          test "verfies an example when it fails" <| \() -> do
            let example = HVE.exampleFromText "1 + 1 ==> 3"
            result <-
              example
                |> HVE.verify
                  Nothing
                  HVE.ModuleInfo
                    { HVE.moduleName = Nothing,
                      HVE.moduleSource = LHE.SrcLoc.noSrcSpan,
                      HVE.imports = [HVE.makeSimpleImport "NriPrelude"],
                      HVE.languageExtensions = []
                    }
                |> Expect.fromIO
            result
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/verify-unverified.hs",
          test "verfies multiline example (succeeds)" <| \() ->
            do
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
                      |> Text.join "\n"
                      |> HVE.exampleFromText
              result <-
                example
                  |> HVE.verify
                    Nothing
                    HVE.ModuleInfo
                      { HVE.moduleName = Nothing,
                        HVE.moduleSource = LHE.SrcLoc.noSrcSpan,
                        HVE.imports =
                          [ HVE.makeSimpleImport "List",
                            HVE.makeSimpleImport "NriPrelude"
                          ],
                        HVE.languageExtensions = []
                      }
                  |> Expect.fromIO
              result
                |> Debug.toString
                |> Expect.equalToContentsOf "test/golden-results/verify-multiline-verified.hs",
          test "verfies multiline example (fails)" <| \() -> do
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
                    |> Text.join "\n"
                    |> HVE.exampleFromText
            result <-
              example
                |> HVE.verify
                  Nothing
                  HVE.ModuleInfo
                    { HVE.moduleName = Nothing,
                      HVE.moduleSource = LHE.SrcLoc.noSrcSpan,
                      HVE.imports =
                        [ HVE.makeSimpleImport "List",
                          HVE.makeSimpleImport "NriPrelude"
                        ],
                      HVE.languageExtensions = []
                    }
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
                          |> HVE.comments
                          |> HVE.examples
                          |> Prelude.traverse (HVE.verify (Just modulePath) (HVE.moduleInfo parsed))
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
