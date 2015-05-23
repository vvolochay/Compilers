module Main where

import FCC.Parser
import FCC.Typecheck
import FCC.Codegen
import FCC.Stdlib
import FCC.Optimize

import Data.List (intercalate)

main :: IO ()
main = do
  input <- getContents
  let p = parse input
  case p of
   Left e -> putStrLn $ "failed to parse: " ++ show e
   Right x -> case runTC (withStdlib x) of
     Left e' -> putStrLn $ "failed to typecheck: " ++ show e'
     Right p -> let p' = optimize p in (putStrLn $ "@ " ++ show p') >> (putStrLn $ intercalate "\n" $ codegen p')
