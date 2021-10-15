module Main where

import Data.Foldable
import qualified Data.List as List
import NriPrelude
import qualified Haskell.Verify.Examples as HVE
import qualified Paths_haskell_verify_examples as DataPath
import qualified Prelude
import qualified System.IO
import qualified System.IO.Temp

isHaskellStartBlock = List.isPrefixOf "```haskell"
isEndBlock = List.isPrefixOf "```"

runAnonymousBlock :: Prelude.String -> Prelude.IO Prelude.String
runAnonymousBlock s = do
  logHandler <- Platform.silentHandler
  handler <- HVE.handler logHandler
  System.IO.Temp.withSystemTempFile "DocModule.hs" (\filePath handle -> do
      System.IO.hPutStr handle s
      System.IO.hClose handle
      results <- Task.attempt logHandler (do 
          parsed <- HVE.parse handler filePath
          cradleInfo <- HVE.tryLoadImplicitCradle handler filePath
          HVE.verify handler cradleInfo parsed
          )
      
      Prelude.return (Prelude.show results)
      )

gen :: [Prelude.String] -> Prelude.IO [Prelude.String]
gen lines = genLine lines
    where genLine :: [Prelude.String] -> Prelude.IO [Prelude.String]
          genLine [] = Prelude.return []
          genLine (l:ls) = fmap (l:) (if isHaskellStartBlock l 
                                      then genHaskellLine ls []
                                      else genLine ls)
         
          genHaskellLine [] _ = Prelude.return []
          genHaskellLine (l:ls) code = if isEndBlock l
                                       then do
                                           r <- runAnonymousBlock (List.intercalate "\n" code)
                                           fmap (\ls' -> [l, "", "```", r, "```", ""] ++ ls') (genLine ls)
                                       else fmap (l:) (genHaskellLine ls (code ++ [l]))

main :: Prelude.IO ()
main = do
  templatePath <- DataPath.getDataFileName "doc/template.md"
  readmePath <- DataPath.getDataFileName "README.md"

  templateLines <- Prelude.fmap Prelude.lines (Prelude.readFile templatePath)
 
  readmeLines <- gen templateLines

  Prelude.writeFile readmePath (List.intercalate "\n" readmeLines)