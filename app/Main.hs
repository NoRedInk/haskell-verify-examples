module Main where

import qualified Control.Monad
import qualified Data.Foldable
import qualified Haskell.Verify.Examples as HVE
import NriPrelude
import Options.Applicative ((<**>))
import qualified Options.Applicative as OA
import qualified System.Directory
import qualified System.Environment
import System.FilePath.Find ((&&?), (==?))
import qualified System.FilePath.Find as Find
import Prelude ((<>))
import qualified Prelude

data Options = Options
  { files :: List Prelude.FilePath,
    showTodos :: ShowTodos
  }
  deriving (Show)

data ShowTodos = ShowTodos | HideTodos
  deriving (Show)

optionsParser :: OA.Parser Options
optionsParser =
  map2
    Options
    ( OA.many
        <| OA.strOption
          ( OA.metavar "FILES"
              <> OA.help "Specific files to verify"
          )
    )
    ( OA.flag
        HideTodos
        ShowTodos
        ( OA.long "todos"
            <> OA.short 't'
            <> OA.help "Show examples that don't have a verified result yet."
        )
    )

optionsInfo :: OA.ParserInfo Options
optionsInfo =
  OA.info
    (optionsParser <**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "Verify that your examples are correct."
        <> OA.header "haskell-verify-examples"
    )

main :: Prelude.IO ()
main = do
  logHandler <- Platform.silentHandler
  handler <- HVE.handler logHandler
  cwd <- System.Directory.getCurrentDirectory
  params <- System.Environment.getArgs
  options <- OA.execParser optionsInfo
  let _ = Debug.log "opts" options
  files' <- case files options of
    [file] -> Prelude.pure [file]
    [] -> Find.find (noRCS &&? noDist) (Find.extension ==? ".hs") cwd
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
