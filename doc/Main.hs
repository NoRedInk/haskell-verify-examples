module Main where

import Data.Foldable
import qualified Data.List
import qualified Haskell.Verify.Examples as HVE
import qualified List
import NriPrelude
import qualified Result
import qualified System.Environment
import qualified System.IO
import qualified System.IO.Temp
import qualified Prelude

verifyAnonymousBlocks :: List Prelude.String -> Prelude.IO ()
verifyAnonymousBlocks blocks = do
  logHandler <- Platform.silentHandler
  handler <- HVE.handler logHandler
  System.IO.Temp.withSystemTempDirectory "DocModules" <| \dirPath -> do
    results <-
      blocks
        |> List.indexedMap (,)
        |> Prelude.traverse
          ( \(index, block) -> do
              let filePath = dirPath ++ "/Example" ++ Prelude.show index ++ ".hs"
              System.IO.writeFile filePath block
              Task.attempt
                logHandler
                ( do
                    parsed <- HVE.parse handler filePath
                    cradleInfo <- HVE.tryLoadImplicitCradle handler filePath
                    results <- HVE.verify handler cradleInfo parsed HVE.ShowTodos
                    Task.succeed (HVE.moduleInfo parsed, results)
                )
          )
    HVE.report [HVE.Stdout] (Prelude.sequence results)
    Prelude.pure ()

getBlocks :: List Prelude.String -> List Prelude.String -> Prelude.IO (List Prelude.String)
getBlocks [] acc = Prelude.pure acc
getBlocks (l : ls) acc =
  if isHaskellStartBlock l
    then haskellBlock ls [] acc
    else getBlocks ls acc
  where
    isHaskellStartBlock l' =
      -- TODO use a proper markdown parser. Markdown has so many
      -- edge-cases. This might break in some cases.
      Data.List.isPrefixOf "```haskell" l
        || Data.List.isPrefixOf "```hs" l
    isEndBlock = Data.List.isPrefixOf "```"

    haskellBlock [] code acc = Prelude.pure (code : acc)
    haskellBlock (l : ls) code acc =
      if isEndBlock l
        then getBlocks ls (code : acc)
        else haskellBlock ls (code ++ "\n" ++ l) acc

main :: Prelude.IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    [] -> Prelude.pure ()
    readmePath : _ -> do
      readmeLines <- Prelude.fmap Prelude.lines (Prelude.readFile readmePath)
      blocks <- getBlocks readmeLines []
      verifyAnonymousBlocks blocks
