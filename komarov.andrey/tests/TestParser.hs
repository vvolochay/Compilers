import System.Exit
import System.Directory
import Data.List (isPrefixOf, isSuffixOf)
import Control.Monad (forM_)

import FCC.Parser (parse)

checkFile :: FilePath -> IO ()
checkFile path = do
  putStrLn $ "Checking " ++ path
  contents <- readFile path
  let res = parse contents
  print res
  case ("parseErr" `isPrefixOf` path, res) of
   (True, Right _) -> exitFailure
   (False, Left _) -> exitFailure
   _ -> return ()


main :: IO ()
main = do
  setCurrentDirectory "examples"
  files <- getDirectoryContents "."
  let good = filter (".fc" `isSuffixOf`) files
  forM_ good checkFile
