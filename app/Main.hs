module Main where

import qualified Control.Monad
import qualified Data.Foldable
import qualified Haskell.Verified.Examples as HVE
import NriPrelude
import qualified Platform
import qualified System.Directory
import qualified System.Environment
import System.FilePath.Find ((&&?), (==?))
import qualified System.FilePath.Find as Find
import qualified Prelude

main :: Prelude.IO ()
main = do
  doAnything <- Platform.doAnythingHandler
  logHandler <- Platform.silentHandler
  cwd <- System.Directory.getCurrentDirectory
  params <- System.Environment.getArgs
  files <- case params of
    [file] -> Prelude.pure [file]
    [] -> Find.find (noRCS &&? noDist) (Find.extension ==? ".hs") cwd
  results <-
    files
      |> List.map
        ( \modulePath -> do
            parsed <-
              HVE.parse doAnything modulePath
                |> Task.andThen (HVE.tryLoadImplicitCradle doAnything modulePath)
            results <- HVE.verify doAnything parsed
            Task.succeed (HVE.moduleInfo parsed, results)
        )
      |> Task.parallel
      |> Task.attempt logHandler
  case results of
    Ok res -> HVE.report [HVE.Stdout] res
    Err err ->
      let _ = Debug.log "ERR TODO Handle nicely" err
       in Prelude.pure ()

noRCS :: Find.RecursionPredicate
noRCS =
  Prelude.flip
    Control.Monad.liftM
    Find.fileName
    (Prelude.flip Data.Foldable.notElem ["_darcs", "SCCS", "CVS", ".svn", ".hg", ".git"])

noDist :: Find.RecursionPredicate
noDist =
  Prelude.flip
    Control.Monad.liftM
    Find.fileName
    (Prelude.flip Data.Foldable.notElem ["_build", "dist-newstyle", "test"])
