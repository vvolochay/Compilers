import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import System.Exit
import System.Directory

import Data.List

import Control.Monad

import FCC.AST
import FCC.Parser

-- main = defaultMain tests

{-
tests = [
  testGroup "Expression" [
     testProperty "expr" prop_expr
     ]
  ]

prop_expr :: Expression () -> Bool
prop_expr (EDeref _) = False
prop_expr _ = True
-}

checkFile :: FilePath -> IO ()
checkFile path = do
  putStrLn $ "Checking " ++ path
  contents <- readFile path
  let res = parse contents
  print res
  case res of
   Right p -> case parse (show p) of
     Right p' -> when (p /= p') $ print p >> print p' >> exitFailure
     Left _ -> print p >> exitFailure
   Left _ -> return ()

main :: IO ()
main = do
  setCurrentDirectory "examples"
  files <- getDirectoryContents "."
  let good = filter (".fc" `isSuffixOf`) files
  forM_ good checkFile
