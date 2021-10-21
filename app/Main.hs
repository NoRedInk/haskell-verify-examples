{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Control.Monad
import qualified Data.Foldable
import qualified Haskell.Verify.Examples as HVE
import NriPrelude
import System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as CA
import qualified System.Console.CmdArgs.Quote as CA
import qualified System.Directory
import System.FilePath.Find ((&&?), (==?))
import qualified System.FilePath.Find as Find
import qualified Prelude

data Mode
  = Verify
      { show_todos :: Bool,
        files :: List Prelude.FilePath
      }
  | Watch
      {show_todos :: Bool}
  deriving (Prelude.Show, CA.Data, CA.Typeable)

modes :: Mode
modes =
  CA.modes
    [ verifyMode &= CA.name "verify" &= CA.auto,
      watchMode &= CA.name "watch"
    ]

verifyMode :: Mode
verifyMode =
  Verify
    { show_todos = False &= CA.help "Show examples with no expected results.",
      files = [] &= CA.args &= CA.typ "FILES"
    }

watchMode :: Mode
watchMode =
  Watch
    { show_todos = False &= CA.help "Show examples with no expected results."
    }

getMode :: Prelude.IO Mode
getMode = CA.cmdArgs modes

main :: Prelude.IO ()
main = do
  mode <- getMode
  case mode of
    Watch {show_todos} -> Debug.todo "TODO"
    Verify {show_todos, files} -> verify show_todos files

verify :: Bool -> List Prelude.FilePath -> Prelude.IO ()
verify _showTodos files = do
  logHandler <- Platform.silentHandler
  handler <- HVE.handler logHandler
  cwd <- System.Directory.getCurrentDirectory
  files' <- case files of
    [] -> Find.find (noRCS &&? noDist) (Find.extension ==? ".hs") cwd
    xs -> Prelude.pure xs
  results <-
    files'
      |> List.map
        ( \modulePath -> do
            parsed <- HVE.parse handler modulePath
            cradleInfo <- HVE.tryLoadImplicitCradle handler modulePath
            results <- HVE.verify handler cradleInfo parsed
            Task.succeed (HVE.moduleInfo parsed, results)
        )
      |> Task.parallel
      |> Task.attempt logHandler
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
