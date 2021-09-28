-- | Module for presenting example results on the console.
module Haskell.Verified.Examples.Reporter.Stdout (report) where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Haskell.Verified.Examples.Internal
import Haskell.Verified.Examples.Verified (Verified (..))
import qualified Language.Haskell.Exts.SrcLoc as LHE.SrcLoc
import qualified Language.Haskell.Interpreter as Hint
import qualified List
import NriPrelude
import qualified Pretty.Diff as Diff
import qualified Set
import qualified System.Console.Terminal.Size as Terminal
import qualified System.Directory
import System.FilePath ((</>))
import qualified System.IO
import qualified Text
import Text.Colour (Chunk, chunk, faint, underline)
import qualified Text.Colour
import qualified Text.Colour.Capabilities
import qualified Text.Show.Pretty
import qualified Prelude

report :: System.IO.Handle -> Result Error (List (ModuleInfo, List (Example, ExampleResult))) -> Prelude.IO ()
report handle (Ok results) = reportResults handle results
report handle (Err err) = reportError handle err

reportResults :: System.IO.Handle -> List (ModuleInfo, List (Example, ExampleResult)) -> Prelude.IO ()
reportResults handle results = do
  window <- Terminal.size
  let terminalWidth = case window of
        Just Terminal.Window {Terminal.width} -> width - 4 -- indentation
        Nothing -> 80
  terminalCapabilities <- Text.Colour.Capabilities.getTerminalCapabilitiesFromHandle handle
  perModuleResults <- Prelude.traverse (augmentSrc >> map (renderPerModule terminalWidth)) results
  let summary =
        results
          |> List.concatMap Tuple.second
          |> examplesSummary
          |> renderSummary
  (separateBlocks perModuleResults ++ [chunk "\n\n"] ++ summary)
    |> Text.Colour.hPutChunksWith terminalCapabilities handle
  System.IO.hFlush handle

reportError :: System.IO.Handle -> Error -> Prelude.IO ()
reportError handle err = do
  terminalCapabilities <- Text.Colour.Capabilities.getTerminalCapabilitiesFromHandle handle
  err
    |> Debug.toString
    |> chunk
    |> List.singleton
    |> Text.Colour.hPutChunksWith terminalCapabilities handle
  System.IO.hFlush handle

data Summary = Summary
  { verified :: List Example,
    unverified :: List Example,
    noExamples :: List Example,
    evaluationFailed :: List Example
  }

renderSummary :: Summary -> List Chunk
renderSummary Summary {verified, unverified, noExamples, evaluationFailed} =
  List.concat
    [ [ underline
          <| if List.all (List.length >> (==) 0) [unverified, noExamples, evaluationFailed]
            then green (chunk "All examples verified!")
            else red (chunk "Not all examples verified!")
      ],
      [chunk ("\nVerified: " ++ Text.fromInt (List.length verified))],
      renderSummaryForType "Unverified" unverified,
      renderSummaryForType "No examples" noExamples,
      renderSummaryForType "Evaluation failed" evaluationFailed
    ]

renderSummaryForType :: Text -> List Example -> List Chunk
renderSummaryForType prefix examples =
  chunk ("\n" ++ prefix ++ ": " ++ Text.fromInt (List.length examples)) :
  if List.isEmpty examples
    then []
    else
      chunk "\n  In these files:" :
      ( examples
          |> List.map (exampleSrcSpan >> LHE.SrcLoc.srcSpanFilename)
          |> Set.fromList
          |> Set.toList
          |> List.map (\src -> chunk ("\n  * " ++ Text.fromList src))
      )

examplesSummary :: List (Example, ExampleResult) -> Summary
examplesSummary =
  List.foldl
    ( \(example, result) summary ->
        case result of
          ExampleVerifySuccess Verified ->
            summary {verified = example : verified summary}
          ExampleVerifySuccess (Unverified _ _) ->
            summary {unverified = example : unverified summary}
          ExampleVerifySuccess NoExampleResult ->
            summary {noExamples = example : noExamples summary}
          ExampleVerifyFailed _ -> summary {evaluationFailed = example : evaluationFailed summary}
    )
    (Summary [] [] [] [])

augmentSrc :: (ModuleInfo, a) -> Prelude.IO (ModuleInfo, a, Maybe BS.ByteString)
augmentSrc (modInfo, xs) = do
  maybeModule <- readSrc (moduleFilePath modInfo)
  Prelude.pure (modInfo, xs, maybeModule)

renderPerModule :: Int -> (ModuleInfo, List (Example, ExampleResult), Maybe BS.ByteString) -> List Chunk
renderPerModule terminalWidth (modInfo, exampleResults, maybeModule) =
  if examplesVerified (List.map Tuple.second exampleResults)
    then []
    else
      let renderedExampleResults =
            exampleResults
              |> List.map (renderExampleWithSrc terminalWidth maybeModule)
              |> separateBlocks
          header =
            case moduleName modInfo of
              Just name -> "Examples of module " ++ name ++ " unverified."
              Nothing -> "Examples unverified."
       in red (chunk header) : chunk "\n" : renderedExampleResults

renderExampleWithSrc :: Int -> Maybe BS.ByteString -> (Example, ExampleResult) -> List Chunk
renderExampleWithSrc terminalWidth contents result =
  case renderExample terminalWidth result of
    [] -> []
    renderedExamples ->
      List.concat
        [ result
            |> Tuple.first
            |> exampleSrcSpan
            |> renderSrcSpan contents,
          renderedExamples
        ]

renderExample :: Int -> (Example, ExampleResult) -> List Chunk
renderExample _ (example, ExampleVerifyFailed err) =
  case err of
    UnkownLanguageExtension exts ->
      chunk "Unknown extensions:\n" :
      List.map (\ext -> chunk ("* " ++ Text.fromList ext)) exts
    InterpreterError (Hint.UnknownError unknownError) ->
      [ chunk "Unknown error:\n",
        chunk <| Text.fromList unknownError
      ]
    InterpreterError (Hint.WontCompile ghcErrors) ->
      chunk "The example doesn't compile:\n" :
      List.map (chunk << Text.fromList << Hint.errMsg) ghcErrors
    InterpreterError (Hint.NotAllowed msg) ->
      [ chunk "Not allowed:\n",
        chunk <| Text.fromList msg
      ]
    InterpreterError (Hint.GhcException msg) ->
      [ chunk "GHC exception:\n",
        chunk <| Text.fromList msg
      ]
renderExample terminalWidth (example, ExampleVerifySuccess verified) =
  case verified of
    Verified -> []
    Unverified expected actual ->
      let expectedText = Text.fromList (Text.Show.Pretty.ppShow actual)
          actualText = Text.fromList (Text.Show.Pretty.ppShow expected)
          numLines text = List.length (Text.lines text)
       in [ chunk "The example was incorrect and couldn't be verified.",
            chunk "\n",
            chunk
              <| Diff.pretty
                Diff.Config
                  { Diff.separatorText = Just "==>",
                    Diff.wrapping = Diff.Wrap (Prelude.fromIntegral terminalWidth),
                    Diff.multilineContext =
                      if numLines expectedText < 6 && numLines actualText < 6
                        then Diff.FullContext
                        else Diff.Surrounding 2 "..."
                  }
                actualText
                expectedText
          ]
    NoExampleResult ->
      ["No expected result for example."]

readSrc :: Prelude.FilePath -> Prelude.IO (Maybe BS.ByteString)
readSrc srcPath = do
  cwd <- System.Directory.getCurrentDirectory
  let path = cwd </> srcPath
  exists <- System.Directory.doesFileExist path
  if exists
    then do
      contents <- BS.readFile path
      Prelude.pure (Just contents)
    else Prelude.pure Nothing

renderSrcSpan :: Maybe BS.ByteString -> LHE.SrcLoc.SrcSpan -> List Chunk
renderSrcSpan Nothing _ = []
renderSrcSpan (Just contents) span = do
  let startLine = Prelude.fromIntegral (LHE.SrcLoc.srcSpanStartLine span)
  let lines =
        contents
          |> BS.split 10 -- splitting newlines
          |> List.drop (startLine - extraLinesOnFailure - 1)
          |> List.take (extraLinesOnFailure * 2 + 1)
          |> List.indexedMap
            ( \i l ->
                Text.fromInt (startLine + i - extraLinesOnFailure)
                  ++ ": "
                  ++ TE.decodeUtf8 l
            )
  case lines of
    [] -> []
    lines' ->
      List.concat
        [ [ "\n",
            chunk (Text.fromList (LHE.SrcLoc.srcSpanFilename span)),
            ":",
            chunk (Text.fromInt (Prelude.fromIntegral (LHE.SrcLoc.srcSpanStartLine span))),
            "\n"
          ],
          List.indexedMap
            ( \nr line ->
                if nr == extraLinesOnFailure
                  then red (chunk ("✗ " ++ line ++ "\n"))
                  else faint (chunk ("  " ++ line ++ "\n"))
            )
            lines'
        ]

red :: Chunk -> Chunk
red = Text.Colour.fore Text.Colour.red

green :: Chunk -> Chunk
green = Text.Colour.fore Text.Colour.green

extraLinesOnFailure :: Int
extraLinesOnFailure = 5

separateBlocks :: List (List Chunk) -> List Chunk
separateBlocks =
  List.filter (not << List.isEmpty) >> List.intersperse [chunk "\n\n"] >> List.concat
