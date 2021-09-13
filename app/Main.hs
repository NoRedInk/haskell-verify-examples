module Main where

import qualified Control.Monad
import qualified Data.Foldable
import qualified Haskell.Verified.Examples as HVE
import NriPrelude
import qualified System.Directory
import qualified System.Environment
import System.FilePath.Find ((&&?), (==?))
import qualified System.FilePath.Find as Find
import qualified Prelude

main :: Prelude.IO ()
main = do
  cwd <- System.Directory.getCurrentDirectory
  params <- System.Environment.getArgs
  files <- case params of
    [file] -> Prelude.pure [file]
    [] -> Find.find (noRCS &&? noDist) (Find.extension ==? ".hs") cwd
  results <-
    files
      |> Prelude.traverse
        ( \modulePath -> do
            parsed <- HVE.parse modulePath >>= HVE.tryLoadImplicitCradle modulePath
            results <- HVE.verify parsed
            Prelude.pure (HVE.moduleInfo parsed, results)
        )
  HVE.report [HVE.Stdout] results

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
