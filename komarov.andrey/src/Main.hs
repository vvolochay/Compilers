module Main where

import FCC.Parser
import FCC.Typecheck
import FCC.Codegen

import Data.List (intercalate)

main :: IO ()
main = do
  input <- getContents
  let p = parse input
  case p of
   Left e -> putStrLn $ "failed to parse: " ++ show e
   Right x -> case runTC x of
     Left e' -> putStrLn $ "failed to typecheck: " ++ show e'
     Right p -> print p >> (putStrLn $ intercalate "\n" $ codegen p)
