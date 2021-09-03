module Main where

import qualified Control.Monad
import qualified Data.Foldable
import qualified Haskell.Verified.Examples as HVE
import NriPrelude
import qualified System.Directory
import System.FilePath.Find ((&&?), (==?))
import qualified System.FilePath.Find as Find
import qualified Prelude

main :: Prelude.IO ()
main = do
  cwd <- System.Directory.getCurrentDirectory
  files <- Find.find (noRCS &&? noDist) (Find.extension ==? ".hs") cwd
  results <-
    files
      |> Prelude.traverse
        ( \modulePath -> do
            parsed <- HVE.parse modulePath
            result <-
              parsed
                |> HVE.examples
                |> Prelude.traverse (HVE.verify (Just modulePath) (List.filterMap Prelude.id [HVE.moduleName parsed]) (HVE.languageExtensions parsed))
            Prelude.pure (modulePath, result)
        )
  let _ = Debug.log "results" results
  Prelude.pure ()

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
    (Prelude.flip Data.Foldable.notElem ["dist-newstyle", "test"])
