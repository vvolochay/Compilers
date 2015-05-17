module Main where

import FCC.Parser
import FCC.Typecheck

main :: IO ()
main = do
  input <- getContents
  let p = parse input
  case p of
   Left e -> putStrLn $ "failed to parse: " ++ show e
   Right x -> print $ runTC x
